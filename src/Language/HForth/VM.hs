{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StrictData         #-}
module Language.HForth.VM where

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


import           Language.HForth.List


-- * Virtual Machine

-- | A dictionary is a map of named instructions ('Forth's).
type Dict w a = M.Map String (ForthStep w a)

-- | Class of values that can constitute a 'Forth'.
class ForthType a where
    tyShow :: a -> String -- ^ String representation of /a/, pretty printer.
    tyToInt :: a -> Maybe Int -- ^ Coercion, ie. for loop counters.
    tyFromInt :: Int -> a -- ^ Coercion
    tyFromBool :: Bool -> a -- ^ Boolean value represented in /a/, by convention @-1@ and @0@.

tyToInt' :: ForthType a => String -> a -> Int
tyToInt' msg = fromMaybe (error ("NOT-INTEGER: " ++ msg)) . tyToInt

instance ForthType Int where
  tyShow    = show
  tyToInt   = Just
  tyFromInt = id
  tyFromBool t = if t then -1 else 0

instance ForthType Integer where
  tyShow    = show
  tyToInt   = Just . fromInteger
  tyFromInt = fromIntegral
  tyFromBool t = if t then -1 else 0

-- | A data cell, for the data stacks.
data DC a = DC a | DCString String | DCXT String

instance ForthType a => Show (DC a) where
  show dc = case dc of
    DC       a   -> tyShow a
    DCString str -> "STRING:" ++ tickQuotes str
    DCXT     str -> "XT:" ++ str

-- | Extract plain value from 'DC', else error.
dcPlain :: DC a -> Forth w a a
dcPlain dc = case dc of
  DC a -> return a
  _    -> throwError "DC-NOT-VALUE-CELL"

-- | A compilation cell, for the compilation stack.
data CC w a = CCWord String | CCForth (ForthStep w a)

-- | Predicate to see if 'CC' is a particular 'CCWord'.
ccIsWord :: String -> CC w a -> Bool
ccIsWord w cw = case cw of
  CCWord w' -> w == w'
  _         -> False

-- | The machine is either interpreting or compiling.
data VMMode = Interpret | Compile deriving stock (Eq,Show)

-- | The machine, /w/ is the type of the world, /a/ is the type of the stack elements.
data VM w a = VM
  { stack     :: [DC a] -- ^ The data stack, /the/ stack.
  , rstack    :: [DC a] -- ^ The return stack.
  , cstack    :: [CC w a] -- ^ The compilation stack.
  , threads   :: M.Map Int ThreadId
  , dict      :: Dict w a -- ^ The dictionary.
  , locals    :: [Dict w a] -- ^ The stack of locals dictionaries.
  , buffer    :: String -- ^ The current line of input text.
  , mode      :: VMMode -- ^ Basic state of the machine.
  , world     :: w -- ^ The world, instance state.
  , literal   :: String -> Maybe a -- ^ Read function for literal values.
  , recursive :: Maybe (String -> ForthStep w a) -- ^ Allow recursive definitions
  , inputPort :: Maybe Handle
  , tracing   :: Int
  , sigint    :: MVar Bool -- ^ True if a SIGINT signal (user interrupt) has been received.
  }

instance ForthType a => Show (VM w a) where
  show vm = concat
    [ "\n DATA STACK: "
    , unwords (map show (reverse $ stack vm))
    , "\n RETURN STACK: "
    , unwords (map show (reverse $ rstack vm))
    , "\n COMPILE STACK DEPTH: "
    , show (length (cstack vm))
    , "\n THREADS: "
    , intercalate "," (map show (M.keys (threads vm)))
    , "\n DICT: "
    , unwords (M.keys (dict vm))
    , "\n LOCALS: "
    , intercalate "," (map (unwords . M.keys) (locals vm))
    , "\n BUFFER: "
    , buffer vm
    , "\n MODE: "
    , show (mode vm)
    , "\n RECURSIVE: "
    , maybe "NO" (const "YES") (recursive vm)
    , "\n INPUT PORT: "
    , maybe "NO" (const "YES") (inputPort vm)
    , "\n TRACING: "
    , show (tracing vm)
    ]

-- | Signals (exceptions) from 'VM'.
data VMSignal = VMEOF | VMNoInput | VMError String deriving stock (Eq,Show)

-- | An instruction, the implementation of a /word/.
type Forth w a r = CME.ExceptT VMSignal (StateT (VM w a) IO) r

data Continue = Next | Exit deriving stock Show

-- | The result of intpreting an instruction: Left = exit, Right () = continue
type ForthStep w a = Forth w a Continue

next :: ForthStep w a
next = return Next

exit :: ForthStep w a
exit = return Exit



-- | Make an empty (initial) machine.
emptyVm :: w -> (String -> Maybe a) -> MVar Bool -> VM w a
emptyVm w lit sig = VM { stack     = []
                       , rstack    = []
                       , cstack    = []
                       , threads   = M.empty
                       , buffer    = ""
                       , mode      = Interpret
                       , dict      = M.empty
                       , locals    = []
                       , world     = w
                       , literal   = lit
                       , recursive = Nothing
                       , inputPort = Nothing
                       , tracing   = -1
                       , sigint    = sig
                       }

-- | Reset 'VM', on error.
vmReset :: VM w a -> VM w a
vmReset vm = vm { stack  = []
                , rstack = []
                , cstack = []
                , buffer = ""
                , mode   = Interpret
                , locals = []
                }

-- | Type specialised variant of 'get' that checks SIGINT handler.
getVm :: Forth w a (VM w a)
getVm = do
  vm  <- get
  sig <- liftIO (modifyMVar (sigint vm) (\s -> return (False, s)))
  when sig (throwError "VM: SIGINT")
  return vm

-- | Function with 'VM'.
withVm :: (VM w a -> (VM w a, r)) -> Forth w a r
withVm f = getVm >>= \vm -> let (vm', r) = f vm in put vm' >> return r

-- | Procedure with 'VM'.
doWithVm :: (VM w a -> Forth w a (VM w a)) -> Forth w a ()
doWithVm f = getVm >>= (f >=> put)

-- | Change the world.
vmModifyWorld :: (w -> w) -> Forth w a ()
vmModifyWorld f = modify (\vm -> vm { world = f (world vm) })

-- * Error

-- | Tracer, levels are 2 = HIGH, 1 = MEDIUM, 0 = LOW
trace :: Int -> String -> Forth w a ()
trace k msg = do
  vm <- getVm
  when (k <= tracing vm) (void $ writeLn msg)

throwError :: String -> Forth w a r
throwError = CME.throwError . VMError

-- | Reader that raises an /unknown word/ error.
unknownError :: String -> Forth w a r
unknownError s = throwError ("UNKNOWN WORD: " ++ tickQuotes s)

-- | Reader that raises an /unimplemented word/ error.
notImplementedError :: String -> Forth w a r
notImplementedError s = throwError ("UNIMPLEMENTED WORD: " ++ tickQuotes s)


write, writeLn, writeSp :: String -> ForthStep w a
write s = do
  (liftIO . putStr) s
  next
writeLn s = do
  (write . (++ "\n")) s
writeSp s = do
  (write . (++ " ")) s
