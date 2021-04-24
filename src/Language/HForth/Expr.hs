{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StrictData         #-}
module Language.HForth.Expr where

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

-- * Token / Expr

-- | Expressions are either literals or words.
data Expr a = Literal a | Word String deriving stock (Show,Eq)

-- | Pretty print 'Expr'.
exprPp :: ForthType a => Expr a -> String
exprPp e = case e of
  Literal a  -> tyShow a
  Word    nm -> nm

-- | Dictionary lookup, word should be lower case.
lookupWord :: String -> VM w a -> Maybe (ForthStep w a)
lookupWord k vm = case locals vm of
  []    -> M.lookup k (dict vm)
  l : _ -> case M.lookup k l of
    Nothing -> M.lookup k (dict vm)
    r       -> r

-- | Parse a token string to an expression.
parseToken :: String -> Forth w a (Expr a)
parseToken s = do
  vm <- getVm
  case lookupWord s vm of
    Just _  -> return (Word s)
    Nothing -> case literal vm s of
      Just l  -> return (Literal l)
      Nothing -> case recursive vm of
        Just _  -> return (Word s) -- if there is a recursive placeholder, defer...
        Nothing -> unknownError s

-- | Read buffer until predicate holds, if /pre/ delete preceding white space.
readUntil :: Bool -> (Char -> Bool) -> Forth w a (String, String)
readUntil pre cf = do
  vm <- getVm
  let f = if pre then dropWhile isSpace else id
      r = breakOn cf (f (buffer vm))
  trace 2 (show ("READUNTIL", mode vm, fst r, length (snd r)))
  put vm { buffer = snd r }
  return r

scanUntil :: (Char -> Bool) -> Forth w a String
scanUntil = fmap fst . readUntil False

-- | Scan a token from 'buffer', ANS Forth type comments are
-- discarded.  Although 'buffer' is filled by 'hGetLine' it may
-- contain newline characters because we may include a file.
scanToken :: Forth w a (Maybe String)
scanToken = do
  r <- readUntil True isSpace
  case r of
    ("", "") -> do
      vm <- getVm
      let sl = length (stack vm)
      writeLn (" OK " ++ if sl == 0 then "" else show sl) >> return Nothing
    (""  , rhs) -> throwError ("SCANTOKEN: NULL: " ++ rhs)
    ("\\", _  ) -> scanUntil (== '\n') >> scanToken
    ("(" , _  ) -> scanUntil (== ')') >> scanToken
    (e   , _  ) -> return (Just e)

-- | Read line from 'inputPort' to 'buffer'.  There are two
-- /exceptions/ thrown here, 'VMEOF' if an input port is given but
-- returns EOF, and 'VMNoInput' if there is no input port.
fwRefill :: ForthStep w a
fwRefill = do
  vm <- getVm
  case inputPort vm of
    Nothing -> CME.throwError VMNoInput
    Just h  -> do
      eof <- liftIO (hIsEOF h)
      when eof (CME.throwError VMEOF)
      trace 2 "REFILL"
      x <- liftIO (hGetLine h)
      put (vm { buffer = x })
      next

-- | If 'scanToken' is 'Nothing', then 'fwRefill' and retry.  Tokens are lower case.
readToken :: Forth w a String
readToken = do
  r <- scanToken
  case r of
    Just str -> return (map toLower str)
    Nothing  -> fwRefill >> readToken

-- | 'parseToken' of 'readToken'.
readExpr :: Forth w a (Expr a)
readExpr = parseToken =<< readToken
