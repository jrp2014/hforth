{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StrictData         #-}
module Language.HForth.Stack where

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

-- * Stack

push' :: DC a -> ForthStep w a
push' x = do
  modify (\vm -> vm { stack = x : stack vm })
  next

-- | Push value onto 'stack'.
push :: a -> ForthStep w a
push = push' . DC

pushr' :: DC a -> ForthStep w a
pushr' x = do
  modify (\vm -> vm { rstack = x : rstack vm })
  next

-- | Push value onto 'rstack'.
pushr :: a -> ForthStep w a
pushr = pushr' . DC

-- | Push value onto 'cstack'.
pushc :: CC w a -> ForthStep w a
pushc x = do
  modify (\vm -> vm { cstack = x : cstack vm })
  next

-- | Pop indicated 'VM' stack.
popVmStack
  :: String -> (VM w a -> [r]) -> (VM w a -> [r] -> VM w a) -> Forth w a r
popVmStack nm f g = do
  vm <- getVm
  case f vm of
    []     -> throwError (nm ++ ": STACK UNDERFLOW")
    x : xs -> put (g vm xs) >> return x

pop' :: Forth w a (DC a)
pop' = popVmStack "DATA" stack (\vm s -> vm { stack = s })

-- | Remove value from 'stack'.
pop :: Forth w a a
pop = pop' >>= dcPlain

popInt :: ForthType a => String -> Forth w a Int
--popInt msg =  pop <&> tyToInt' msg
popInt msg = tyToInt' msg <$> pop

popr' :: Forth w a (DC a)
popr' = popVmStack "RETURN" rstack (\vm s -> vm { rstack = s })

-- | Remove value from 'rstack'.
popr :: Forth w a a
popr = popr' >>= dcPlain

-- | Remove value from 'cstack'.
popc :: Forth w a (CC w a)
popc = popVmStack "COMPILE" cstack (\vm s -> vm { cstack = s })

-- | ( id len -- )
popString :: String -> Forth w a String
popString msg = do
  vm <- getVm
  case stack vm of
    DC _ : DCString str : s' -> put vm { stack = s' } >> return str
    _                        -> throwError ("NOT-STRING?" ++ msg)
