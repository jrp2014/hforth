{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}

-- |
-- Copyright: (c) 2021 jrp2014
-- SPDX-License-Identifier: MIT
-- Maintainer: jrp2014 <jrp2014@users.noreply.github.com>
--
-- See README for more info
-- Translated from
-- http://www.w3group.de/course01.html
module HForth
  ( projectName,
  )
where

import Control.Monad.Fix (MonadFix)
import Control.Monad.State
  ( MonadIO (..),
    MonadState (..),
    StateT (StateT),
    execStateT,
    gets,
    liftIO,
    modify,
  )
import Data.List (find)
import System.IO (BufferMode (NoBuffering), hFlush, hSetBuffering, stdin, stdout)
import Text.Read (readMaybe)

projectName :: String
projectName = "hforth"

-- Parsing
type Token = String

-- | parse a lines into tokens
parse :: [String] -> [Token]
parse = words . unlines

-- VM

type Stack a = [a]

type Dictionary = [XT]

data Hardware = Hardware
  { stack :: Stack Int,
    dictionary :: Stack XT
  }

printHardware :: Hardware -> String
printHardware hw =
  "Hardware {" ++ show (stack hw) ++ "} " ++ show (map name (dictionary hw))

type Label = Int

type MachineState a = StateT Hardware IO a

newtype VM a = VM {unVM :: MachineState a}
  deriving newtype (MonadFix, Monad, MonadIO, Applicative, Functor, MonadState Hardware)

-- | Execution Token
data XT = XT
  { name :: Token,
    prim :: VM ()
  }

ok :: VM ()
ok = do
  st <- gets stack
  liftIO $ print st
  liftIO $ putStr "ok> "
  liftIO $ hFlush stdout

push :: Int -> VM ()
push value = modify (\hw -> hw {stack = value : stack hw})

pop :: VM Int
pop = do
  st <- gets stack
  case st of
    [] -> error "Data stack underrun: pop"
    (x : xs) -> do
      modify (\hw -> hw {stack = xs})
      return x

binOp :: (Int -> Int -> Int) -> VM ()
binOp op = do
  st <- gets stack
  case st of
    (x : y : rest) -> do
      modify (\hw -> hw {stack = y `op` x : rest})
      return ()
    _ -> error "Data underrun: fmul"

fMul :: VM ()
fMul = binOp (*)

fAdd :: VM ()
fAdd = binOp (+)

fHelloWorld :: VM ()
fHelloWorld = liftIO $ putStrLn "Hello World"

addWord :: Token -> VM () -> VM ()
addWord n p = do
  dict <- gets dictionary
  modify (\hw -> hw {dictionary = XT n p : dict})

fDrop :: VM ()
fDrop = do
  pop
  return ()

fWords :: VM ()
fWords = do
  d <- gets dictionary
  liftIO $ print $ name <$> d

fDot :: VM ()
fDot = do
  stk <- gets stack
  if null stk
    then do
      liftIO $ putStrLn "Empty stack"
    else do
      top <- pop
      liftIO $ print top

registerPrimitives :: VM ()
registerPrimitives = do
  addWord "+" fAdd
  addWord "*" fMul
  addWord "hello" fHelloWorld
  addWord "drop" fDrop
  addWord "words" fWords
  addWord "." fDot

interpret :: Token -> VM ()
interpret t = do
  dict <- gets dictionary
  case find ((== t) . name) dict of
    Just xt -> prim xt
    Nothing -> case (readMaybe t :: Maybe Int) of
      Just x -> do
        push x
      Nothing -> do
        liftIO $ putStrLn $ "Word " ++ t ++ " not found"

initialize :: VM ()
initialize = do
  registerPrimitives
  repl

repl :: VM ()
repl = do
  ok
  line <- liftIO getLine
  if line == "q"
    then return ()
    else do
      mapM_ interpret (words line)
      repl

initialState :: Hardware
initialState = Hardware [] []

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  result <- execStateT (unVM initialize) initialState
  putStrLn $ printHardware result
  return ()
