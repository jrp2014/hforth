{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StrictData         #-}
module Language.HForth.Compiler where

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
import           Language.HForth.List
import           Language.HForth.Expr
import           Language.HForth.Dictionaries
import           Language.HForth.Locals



-- * Compiler

-- | ":". Enter compile phase, the word name is pushed onto the
-- /empty/ 'cstack', and a 'locals' frame is added.
fwColon :: ForthStep w a
fwColon = do
  nm <- readToken
  trace 0 ("DEFINE: " ++ nm)
  let
    edit vm = do
      when (isReservedWord nm) (throwError ("':' RESERVED NAME: " ++ nm))
      unless (null (cstack vm)) (throwError ("':' CSTACK NOT EMPTY: " ++ nm))
      return
        (vm { mode   = Compile
            , cstack = [CCWord nm]
            , locals = M.empty : locals vm
            }
        )
  doWithVm edit
  next

-- | ";".  End compile phase.  There is always a compile 'locals'
-- frame to be removed.
fwSemiColon :: ForthStep w a
fwSemiColon = do
  vm <- getVm
  case reverse (cstack vm) of
    CCWord nm : cw ->
      let instr  = map cwInstr cw
          instr' = if M.null (head (locals vm))
            then instr
            else bracketed (beginLocals, endLocals) instr
          w = forthBlock instr'
      in  do
            trace 2 ("END DEFINITION: " ++ nm)
            when (M.member nm (dict vm)) (void $ writeLn ("REDEFINED " ++ nm))
            put
              (vm { cstack = []
                  , locals = tail (locals vm)
                  , dict   = M.insert nm w (dict vm)
                  , mode   = Interpret
                  }
              )
            next
    _ -> throwError "CSTACK UNDERFLOW"
