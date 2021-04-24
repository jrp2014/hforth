{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StrictData         #-}
module Language.HForth.Operation where

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
import           Language.HForth.Compile


-- * Operation

execErr :: VM w a -> ForthStep w a -> IO (VM w a)
execErr vm fw = do
  (r, vm') <- runStateT (CME.runExceptT fw) vm
  case r of
    Left  err        -> error ("EXECERR: " ++ show err)
    Right Next -> return vm'
    Right Exit -> return vm'

-- | Read, evaluate, print, loop.  Prints @OK@ at end of line.  Prints
-- error message and runs 'vmReset' on error.
repl' :: (Eq a, ForthType a) => VM w a -> IO ()
repl' vm = do
  (r, vm') <- runStateT (CME.runExceptT vmExecute) vm
  case r of
    Left err -> case err of
      VMEOF       -> putStrLn "BYE" >> liftIO exitSuccess
      VMNoInput   -> liftIO exitSuccess
      VMError msg -> putStrLn (" ERROR: " ++ msg) >> repl' (vmReset vm)
    Right Next -> repl' vm'
    Right Exit -> repl' vm'

catchSigint :: VM w a -> IO ()
catchSigint vm = do
  let h = modifyMVar_ (sigint vm) (return . const True)
  _ <- P.installHandler P.sigINT (P.Catch h) Nothing
  _ <- P.installHandler P.sigTERM (P.Catch h) Nothing
  return ()

-- | 'repl'' but with 'catchSigint'.
repl :: (ForthType a, Eq a) => VM w a -> ForthStep w a -> IO ()
repl vm initF = do
  catchSigint vm
  (r, vm') <- runStateT (CME.runExceptT initF) vm
  case r of
    Left err -> case err of
      VMEOF       -> putStrLn "BYE" >> liftIO exitSuccess
      VMNoInput   -> liftIO exitSuccess
      VMError msg -> putStrLn (" ERROR: " ++ msg) >> repl' (vmReset vm)
    Right Next -> repl' vm'
    Right Exit -> repl' vm'

loadFiles :: (Eq a, ForthType a) => [String] -> ForthStep w a
loadFiles nm = do
  trace 0 ("LOAD-FILES: " ++ intercalate "," nm)
  mapM_ fwIncluded' nm
  next
