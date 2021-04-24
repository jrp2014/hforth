module Language.HForth
  ( Language.HForth.Compile
  , Language.HForth.Compiler
  , Language.HForth.Dictionaries
  , Language.HForth.Expr
  , Language.HForth.Interpret
  , Language.HForth.List
  , Language.HForth.Locals
  , Language.HForth.Operation
  , Language.HForth.Stack
  , Language.HForth.Strings
  , Language.HForth.VM
  , Language.HForth.Words
  ) where

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


import           Language.HForth.Compile
import           Language.HForth.Compiler
import           Language.HForth.Dictionaries
import           Language.HForth.Expr
import           Language.HForth.Interpret
import           Language.HForth.List
import           Language.HForth.Locals
import           Language.HForth.Operation
import           Language.HForth.Stack
import           Language.HForth.Strings
import           Language.HForth.VM
import           Language.HForth.Words
