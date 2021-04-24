{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StrictData         #-}
module Language.HForth.Locals where

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
import           Language.HForth.Expr
import           Language.HForth.Dictionaries




-- * LOCALS

-- | Variant on @(local)@, argument not on stack.
fwLocal' :: String -> ForthStep w a
fwLocal' nm = do
  vm <- getVm
  case stack vm of
    e : s' -> do
      put vm
        { stack  = s'
        , locals = case locals vm of
                     []     -> error "NO LOCALS FRAME"
                     l : l' -> M.insert nm (push' e) l : l'
        }
      next
    _ -> throwError ("(LOCAL): STACK UNDERFLOW: " ++ nm)

-- | Function over current locals 'Dict'.
atCurrentLocals :: (Dict w a -> Dict w a) -> VM w a -> VM w a
atCurrentLocals f vm = case locals vm of
  l : l' -> vm { locals = f l : l' }
  _      -> error "ATCURRENTLOCALS"

-- | 'locals' is used both during compilation and interpretation.  In
-- compilation the RHS is undefined, it is used for name lookup and to
-- know if an interpreter 'locals' frame must be made.  In
-- interpretation, if required, it is a secondary dictionary,
-- consulted first.
--fwOpenBrace :: ForthType a => ForthStep w a
fwOpenBrace :: ForthStep w a
fwOpenBrace = do
  let getNames r = do
        w <- readToken
        if w == "}" then return r else getNames (w : r)
  nm <- getNames []
  when (any isReservedWord nm)
       (throwError ("FWOPENBRACE: RESERVED WORD: " ++ unwords nm))
  trace 0 ("DEFINE-LOCALS: " ++ unwords nm)
  let locals' = M.fromList (zip nm (repeat undefined))
  withVm (\vm -> (atCurrentLocals (M.union locals') vm, ()))
  pushc (CCForth (forthBlock (map fwLocal' nm)))
