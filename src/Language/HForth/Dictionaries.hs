{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StrictData         #-}
module Language.HForth.Dictionaries where

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
import           Language.HForth.Words


-- * Dictionaries

coreDict :: (Ord a, Num a, ForthType a) => Dict w a
coreDict =
  let err nm =
        throwError (tickQuotes nm ++ ": compiler word in interpeter context")
  in
    M.fromList
      [ (":"       , fwColon)
      , (";"       , err ";")
      , ("s\""     , fwSQuoteInterpet)
      , ("included", fwIncluded)
      , ("type"    , fwType)
      , ("do"      , err "do")
      , ("i"       , err "i")
      , ("j"       , err "j")
      , ("loop"    , err "loop")
      , ("if"      , err "if")
      , ("else"    , err "else")
      , ("then"    , err "then")
      , ("{"       , err "{")
      , ("}"       , err "}")
      , ("'"       , fwQuote)
      , ("execute" , fwExecute)
      , ("fork"    , fwFork)
      , ("kill"    , fwKill)
      , ("killall" , fwKillAll)
      , ("bye"     , fwBye)
      , ("exit"    , err "exit")
      , ("?exit"   , err "?exit")
  -- STACK
      , ("dup"     , fwDup)
      , ("swap"    , fwSwap)
      , ("drop"    , fwDrop)
      , ("over"    , fwOver)
      , ("rot"     , fwRot)
      , ("2dup"    , fw2Dup)
      , ("?dup"    , err "?dup")
      , (">r"      , fwGTR)
      , ("r>"      , fwRGT)
      , ("0="      , fw0EQ)
      , ("0<"      , fw0LT)
      , ("1+"      , fw1Plus)
      , ("1-"      , fw1Minus)
      , ("roll"    , fwRoll)
      , ("pick"    , fwPick)
      , ("nip"     , fwNip)
      , ("+"       , fwPlus)
      , ("-"       , fwMinus)
      , ("="       , fwEQ)
      , ("<"       , fwLT)
      , (">"       , fwGT)
      , ("(/mod"   , fwOPSlashMod)
      , ("10*"     , fw10Times)
      , ("(10u/mod", fwOP10uSlashMod)
      , ("10u/mod" , fw10uSlashMod)
      , ("(u."     , fwOPuDot)
   -- IO
      , ("emit"    , fwEmit)
      , ("."       , fwDot)
      , (".s"      , fwDotS)
      , ("key", liftIO getChar >>= \c -> push (tyFromInt (fromEnum c)))
  -- DEBUG
      , ("vmstat"  , fwVmStat)
      , ( "trace"
        , pop >>= \k ->
          withVm (\vm -> (vm { tracing = tyToInt' "TRACE" k }, Next))
        )
      ]

coreWords :: [String]
coreWords = M.keys (coreDict :: Dict w Integer)

isReservedWord :: String -> Bool
isReservedWord nm = nm `elem` coreWords
