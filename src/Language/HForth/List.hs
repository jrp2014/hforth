{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StrictData         #-}
module Language.HForth.List where

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



-- * List functions

-- | Read until /f/ is 'True', discarding /x/, RHS may be @[]@.
--
-- > breakOn isSpace "" == ([],[])
-- > breakOn (== ')') "comment ) WORD" == ("comment "," WORD")
-- > breakOn (== '\n') " comment\n\n" == (" comment","\n")
breakOn :: (a -> Bool) -> [a] -> ([a], [a])
breakOn f l = case break f l of
  (lhs, []     ) -> (lhs, [])
  (lhs, _ : rhs) -> (lhs, rhs)

-- | 'snd' of 'breakOn'.
deleteUntil :: (a -> Bool) -> [a] -> [a]
deleteUntil f = snd . breakOn f

bracketed :: (a, a) -> [a] -> [a]
bracketed (l, r) x = l : x ++ [r]

tickQuotes :: String -> String
tickQuotes = bracketed ('\'', '\'')
