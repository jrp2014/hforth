{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StrictData         #-}
module Language.HForth.Words where

import           Control.Concurrent             ( MVar
                                                , ThreadId
                                                , forkIO
                                                , killThread
                                                , modifyMVar
                                                , modifyMVar_
                                                )
import           Control.Monad                  ( (>=>)
                                                , unless
                                                , void
                                                , when
                                                )
import qualified Control.Monad.Except          as CME
import           Control.Monad.State            ( MonadIO(liftIO)
                                                , MonadState(get, put)
                                                , StateT(runStateT)
                                                , modify
                                                )
import           Data.Char                      ( isSpace
                                                , ord
                                                , toLower
                                                )
import           Data.Hashable                  ( Hashable(hash) )
import           Data.List                      ( intercalate )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           System.Directory               ( doesFileExist )
import           System.Exit                    ( die
                                                , exitSuccess
                                                )
import           System.IO                      ( Handle
                                                , hGetLine
                                                , hIsEOF
                                                )
import qualified System.Posix.Signals          as P

import           Language.HForth.VM
import           Language.HForth.Stack
import           Language.HForth.List
import           Language.HForth.Expr
import           Language.HForth.Interpret
import           Language.HForth.Compile


-- * Forth words

-- | Store current buffer & input port, place input string on buffer
-- with no input port, 'vmExecuteBuffer', restore buffer & port.
fwEvaluate' :: (Eq a, ForthType a) => String -> ForthStep w a
fwEvaluate' str = do
  vm <- getVm
  let buf = buffer vm
      ip  = inputPort vm
  vm' <- liftIO (vmExecuteBuffer (vm { buffer = str, inputPort = Nothing }))
  put (vm' { buffer = buf, inputPort = ip })
  next

-- | Variant on @included@, argument not on stack.
fwIncluded' :: (Eq a, ForthType a) => FilePath -> ForthStep w a
fwIncluded' nm = do
  trace 0 ("INCLUDED: " ++ nm)
  x <- liftIO (doesFileExist nm)
  unless x (throwError ("INCLUDED': FILE MISSING: " ++ tickQuotes nm))
  liftIO (readFile nm) >>= fwEvaluate'

fwIncluded :: (Eq a, ForthType a) => ForthStep w a
fwIncluded = popString "INCLUDED" >>= fwIncluded'

fwI :: ForthStep w a
fwI = popr >>= \x -> pushr x >> push x

-- | Forth word @j@.
fwJ :: ForthStep w a
fwJ = do
  x     <- popr
  y     <- popr
  z     <- popr
  _next <- pushr z
  _next <- pushr y
  _next <- pushr x
  push z

-- Apply comparison with top of stack
-- (optimized version of comparisonOp)  Not used
comparison :: ForthType a => (a -> Bool) -> ForthStep w a
comparison cmp = do
  vm <- getVm
  case stack vm of
    DC n : s' -> do
      let flag = tyFromBool $ cmp n in put vm { stack = DC flag : s' }
      next
    _ -> throwError "comparison"

-- | Apply comparison with top of stack
binOp :: (a -> a -> a) -> ForthStep w a
binOp op = do
  vm <- getVm
  case stack vm of
    DC x : DC y : s' -> do
      put vm { stack = DC (y `op` x) : s' }
      next
    _ -> throwError "binop"

-- | Binary stack operation.  The first value on the stack is the RHS.
binaryOp :: (a -> a -> a) -> ForthStep w a
binaryOp f = pop >>= \y -> pop >>= \x -> push (f x y)

-- | Unary stack operation.
unaryOp :: (a -> a) -> ForthStep w a
unaryOp f = pop >>= push . f

-- | comparison operation
comparisonOp :: ForthType a => (a -> a -> Bool) -> ForthStep w a
comparisonOp f = binaryOp (\x y -> tyFromBool (f x y))

predicateOp :: ForthType a => (a -> Bool) -> ForthStep w a
predicateOp f = unaryOp (tyFromBool . f)

-- | dup : ( p -- p p ) swap : ( p q -- q p ) drop : ( p -- ) over : (
-- p q -- p q p ) rot : ( p q r -- q r p ) 2dup : ( p q -- p q p q )
fwDup, fwSwap, fwDrop, fwOver, fwRot, fw2Dup :: ForthStep w a
fwDup = pop' >>= \e -> push' e >> push' e
fwSwap = pop' >>= \p -> pop' >>= \q -> push' p >> push' q
fwDrop = do
  trace 3 "FWDROP"
  _dropped <- pop'
  next
fwOver = pop' >>= \p -> pop' >>= \q -> push' q >> push' p >> push' q
fwRot =
  pop' >>= \p -> pop' >>= \q -> pop' >>= \r -> push' q >> push' p >> push' r
fw2Dup = pop' >>= \p -> pop' >>= \q -> push' q >> push' p >> push' q >> push' p

fwQDup :: (Eq a, ForthType a) => ForthStep w a
fwQDup = forthBlock [fwDup, fwDup, fwQExit, fwDrop]

-- | fwGTR (>r)
fwGTR :: ForthStep w a
fwGTR = pop' >>= pushr'

-- | fwRGT (r>)
fwRGT :: ForthStep w a
fwRGT = popr' >>= push'

-- | 0=
fw0EQ :: (Eq a, ForthType a) => ForthStep w a
fw0EQ = predicateOp (== tyFromInt 0)

-- | 0<
fw0LT :: (Ord a, ForthType a) => ForthStep w a
fw0LT = predicateOp (< tyFromInt 0)

-- | 1+
fw1Plus :: (Num a, ForthType a) => ForthStep w a
fw1Plus = unaryOp (\x -> x + tyFromInt 1)

-- | 1-
fw1Minus :: (Num a, ForthType a) => ForthStep w a
fw1Minus = unaryOp (\x -> x - tyFromInt 1)

-- | roll
fwRoll :: (Eq a, Num a, ForthType a) => ForthStep w a
--fwRoll = fwQDup >> fw0EQ >> interpretIf
--  (next, fwSwap >> fwGTR >> fw1Minus >> fwRoll >> fwRGT >> fwSwap)
fwRoll = forthBlock
  [fwQDup, fw0EQ, fwQExit, fwSwap, fwGTR, fw1Minus, fwRoll, fwRGT, fwSwap]

-- | ( xu ... x1 x0 u -- xu ... x1 x0 xu )
fwPick :: ForthType a => ForthStep w a
fwPick = do
  vm <- getVm
  case stack vm of
    DC n : s' ->
      let n' = tyToInt' "PICK" n
          e  = s' !! n' -- unsafe
      in  put vm { stack = e : s' } >> next
    _ -> throwError "PICK"

-- | nip
fwNip :: ForthStep w a
fwNip = fwSwap >> fwDrop

-- | +
fwPlus :: (Num a) => ForthStep w a
fwPlus = binaryOp (+)
-- | -
fwMinus :: (Num a) => ForthStep w a
fwMinus = binaryOp (-)

-- | <
fwEQ :: (Ord a, ForthType a) => ForthStep w a
fwEQ = comparisonOp (==)

-- | <
fwLT :: (Ord a, ForthType a) => ForthStep w a
fwLT = comparisonOp (<)

-- | <
fwGT :: (Ord a, ForthType a) => ForthStep w a
fwGT = comparisonOp (>)

-- (/mod
fwOPSlashMod :: (Ord a, Num a, ForthType a) => ForthStep w a
fwOPSlashMod = forthBlock
  [ fwGTR
  , fwOver
  , fwOver
  , fwLT
  , fwRGT
  , fwSwap
  , fwQExit
  , fwGTR
  , fwSwap
  , fwOver
  , fwMinus
  , fwSwap
  , fwRGT
  , fw1Plus
  , fwOPSlashMod
  ]

-- | 10*
fw10Times :: (Num a, ForthType a) => ForthStep w a
fw10Times = unaryOp (* tyFromInt 10)

-- | (10u/mod
fwOP10uSlashMod :: (Ord a, Num a, ForthType a) => ForthStep w a
fwOP10uSlashMod = forthBlock
  [ push 2
  , fwPick
  , fwOver
  , fwGT
  , fw0EQ
  , fwQExit
  , fwDup
  , fwGTR
  , fw10Times
  , fwOP10uSlashMod
  , fwSwap
  , fwGTR
  , push 0
  , fwOPSlashMod
  , fwNip
  , fwRGT
  , fw10Times
  , fwPlus
  , fwRGT
  ]

-- | 10u/mod
fw10uSlashMod :: (Ord a, Num a, ForthType a) => ForthStep w a
fw10uSlashMod = forthBlock [push 0, push 1, fwOP10uSlashMod, fwDrop]

-- | (u.
fwOPuDot :: (Ord a, Num a, ForthType a) => ForthStep w a
fwOPuDot = forthBlock
  [ fwQDup
  , fw0EQ
  , fwQExit
  , fw10uSlashMod
  , fwOPuDot
  , push (tyFromInt $ ord '0')
  , fwPlus
  , fwEmit
  ]

fwEmit, fwDot :: ForthType a => ForthStep w a
fwEmit = write . return . toEnum =<< popInt "EMIT"
fwDot = writeSp . show =<< pop'

fwDotS :: ForthType a => ForthStep w a
fwDotS = do
  vm <- getVm
  let l = map show (reverse (stack vm))
      n = "<" ++ show (length l) ++ "> "
  write (n ++ concatMap (++ " ") l)

fwBye :: ForthStep w a
fwBye = liftIO exitSuccess

pushStr :: ForthType a => String -> ForthStep w a
pushStr str =
  let f vm =
        ( vm { stack = DC (tyFromInt (length str)) : DCString str : stack vm }
        , Next
        )
  in  withVm f

fwVmStat :: ForthType a => ForthStep w a
fwVmStat = getVm >>= writeLn . show

fwFork :: ForthType a => ForthStep w a
fwFork = do
  nm <- readToken
  vm <- getVm
  case lookupWord nm vm of
    Just fw -> do
      th <- liftIO (forkIO (void (execErr vm fw)))
      let k = hash th :: Int
      put vm { stack   = DC (tyFromInt k) : stack vm
             , threads = M.insert k th (threads vm)
             }
      next
    Nothing -> throwError ("FORK: UNKNOWN WORD: " ++ nm)

fwKill :: ForthType a => ForthStep w a
fwKill = do
  k  <- popInt "KILL: PID?"
  vm <- getVm
  let threads' = threads vm
  case M.lookup k threads' of
    Nothing -> throwError ("KILL: UNKNOWN THREAD: " ++ show k)
    Just th -> do
      liftIO (killThread th) >> put vm { threads = M.delete k threads' }
      next

fwKillAll :: ForthStep w a
fwKillAll = do
  vm <- getVm
  let th = M.elems (threads vm)
  liftIO (mapM_ killThread th)
  put vm { threads = M.empty }
  next

fwQuote :: ForthStep w a
fwQuote = do
  tok <- readToken
  push' (DCXT tok)

fwExecute :: ForthStep w a
fwExecute = do
  c <- pop'
  case c of
    DCXT xt -> interpretWord xt >> next
    _       -> throwError "EXECUTE: NOT EXECUTION TOKEN"

