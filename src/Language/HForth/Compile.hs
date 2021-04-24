{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StrictData         #-}
module Language.HForth.Compile where

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

import           Language.HForth.Compiler
import           Language.HForth.Expr
import           Language.HForth.Interpret
import           Language.HForth.List
import           Language.HForth.Stack
import           Language.HForth.VM


-- * Compile

-- | Define word and add to dictionary.  The only control structures are /if/ and /do/.
vmCompile :: (Eq a, ForthType a) => ForthStep w a
vmCompile = do
  expr <- readExpr
  trace 2 ("COMPILE: " ++ exprPp expr)
  case expr of
    Word ";"     -> fwSemiColon
    Word ":"     -> throwError ": IN COMPILE CONTEXT"
    Word "do"    -> pushc (CCWord "do")
    Word "i"     -> pushc (CCForth fwI)
    Word "j"     -> pushc (CCForth fwJ)
    Word "loop"  -> fwLoop
    Word "if"    -> pushc (CCWord "if")
    Word "else"  -> pushc (CCWord "else")
    Word "then"  -> fwThen
    Word "{"     -> fwOpenBrace
    Word "s\""   -> fwSQuoteCompiler
    Word "exit"  -> pushc (CCForth fwExit)
    Word "?exit" -> pushc (CCForth fwQExit)
    Word "?dup"  -> pushc (CCForth fwQDup)
    e            -> pushc (CCForth (interpretExpr e))

-- | Get instruction at 'CC' or raise an error.
cwInstr :: CC w a -> ForthStep w a
cwInstr cw = case cw of
  CCWord  w -> throwError ("cwInstr: WORD: " ++ w)
  CCForth f -> f

-- | Type specialised 'foldl1' of '>>'.
forthBlock :: [ForthStep w a] -> ForthStep w a
forthBlock []       = next
forthBlock (i : is) = i >>= \case
  Exit -> next
  Next -> forthBlock is

--forthBlock = foldl1 (>>)

-- | Add a 'locals' frame.
beginLocals :: ForthStep w a
beginLocals = withVm (\vm -> (vm { locals = M.empty : locals vm }, Next))

-- | Remove a 'locals' frame.
endLocals :: ForthStep w a
endLocals = withVm (\vm -> (vm { locals = tail (locals vm) }, Next))

-- | Unwind the 'cstack' to the indicated control word.  The result is
-- the code block, in sequence.  The control word is also removed from
-- the cstack.
unwindCstackTo :: String -> Forth w a [CC w a]
unwindCstackTo w = do
  withVm
    (\vm ->
      let (r, c) = break (ccIsWord w) (cstack vm)
      in  (vm { cstack = tail c }, reverse r)
    )

-- | Either 'vmInterpret' or 'vmCompile', depending on 'mode'.
vmExecute :: (Eq a, ForthType a) => ForthStep w a
vmExecute = do
  vm <- getVm
  case mode vm of
    Interpret -> vmInterpret
    Compile   -> vmCompile

vmExecuteBuffer :: (ForthType a, Eq a) => VM w a -> IO (VM w a)
vmExecuteBuffer vm = do
  (r, vm') <- runStateT (CME.runExceptT vmExecute) vm
  case r of
    Left err -> case err of
      VMNoInput   -> return vm'
      VMEOF       -> die "VMEXECUTEBUFFER: VMVOF"
      VMError msg -> die
        (  "VMEXECUTEBUFFER: "
        ++ msg
        ++ " before '"
        ++ head (lines (buffer vm'))
        ++ "'"
        )
    Right Next -> vmExecuteBuffer vm'
    Right Exit -> vmExecuteBuffer vm' -- TODO don't stop buffer execution?

-- * DO LOOP

-- | A loop ends when the two elements at the top of the rstack are equal.
loopEnd :: Eq a => Forth w a Bool
loopEnd = do
  vm <- getVm
  case rstack vm of
    DC p : DC q : _ -> return (p == q)
    _               -> throwError "LOOP-END: ILLEGAL RSTACK"

-- | /code/ is the expressions between @do@ and @loop@.
interpretDoLoop :: (ForthType a, Eq a) => ForthStep w a -> ForthStep w a
interpretDoLoop code = do
  start <- pop
  end   <- pop
  _next <- pushr end
  _next <- pushr start
  let step = do
        shortCircuit <- code
        case shortCircuit of
          Exit -> exit
          Next -> do
            i <- popr
            let i' = tyFromInt (tyToInt' "DO-LOOP: I" i + 1)
            pushr i'
  let loop = do
        r <- loopEnd
        if not r then step >> loop else popr >> popr >> next
  loop

-- | Compile @loop@ statement, end of do block.
fwLoop :: (Eq a, ForthType a) => ForthStep w a
fwLoop = do
  cw <- unwindCstackTo "do"
  let w = forthBlock (map cwInstr cw)
  pushc (CCForth (interpretDoLoop w))

-- | @exit@ statement
fwExit :: ForthStep w a
fwExit = exit -- short circuit

-- | @?exit@ statement
fwQExit :: (Eq a, ForthType a) => ForthStep w a
fwQExit = interpretIf (fwExit, next)

-- * IF ELSE THEN

-- | Consult stack and select either /true/ or /false/.
interpretIf
  :: (Eq a, ForthType a) => (ForthStep w a, ForthStep w a) -> ForthStep w a
interpretIf (t, f) = pop >>= \x -> do
  trace 3 ("INTERPRETIF: " ++ tyShow x)
  if x /= tyFromBool False then t else f

-- | Compile @then@ statement, end of @if@ block.
fwThen :: (Eq a, ForthType a) => ForthStep w a
fwThen = do
  cw <- unwindCstackTo "if"
  let f = forthBlock . map cwInstr
  case break (ccIsWord "else") cw of
    (tb, []) -> pushc (CCForth (interpretIf (f tb, next)))
    (tb, fb) -> pushc (CCForth (interpretIf (f tb, f (tail fb))))
