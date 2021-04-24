{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StrictData         #-}
module Language.HForth.Interpret where

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
import           Language.HForth.Expr
import           Language.HForth.Stack

-- * Interpret

-- | 'lookupWord' in the dictionary
interpretWord :: String -> ForthStep w a
interpretWord w = do
  vm <- getVm
  trace 3 ("INTERPRETWORD: " ++ w)
  case lookupWord w vm of
    Just r  -> r
    Nothing -> unknownError w

{-
    case recursive vm of
      Just f ->
        let dR = f w in put vm { dict = M.insert w dR (dict vm) } >> dR
      Nothing -> unknownError w
-}


-- | Either 'interpretWord' or 'push' literal.
interpretExpr :: Expr a -> ForthStep w a
interpretExpr e = case e of
  Word    w -> interpretWord w
  Literal a -> push a

-- | 'interpretExpr' of 'readExpr'.
vmInterpret :: ForthStep w a
vmInterpret = readExpr >>= interpretExpr
