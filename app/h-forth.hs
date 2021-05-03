{- base -}
import           Control.Concurrent             ( newMVar {- base -}
                                                )
  {- containers -}
import           Data.Ratio                     ( Ratio {- base -}
                                                )

import           HForth                         ( Dict
                                                , ForthType(..)
                                                , VM(..)
                                                , coreDict
                                                , emptyVm
                                                , fwUndefined
                                                , loadFiles
                                                , repl
                                                
                                                )
import           Options.Applicative            ( (<**>)
                                                , ParserInfo
                                                , argument
                                                , auto
                                                , execParser
                                                , fullDesc
                                                , header
                                                , help
                                                , helper
                                                , info
                                                , long
                                                , many
                                                , metavar
                                                , option
                                                , progDesc
                                                , short
                                                , showDefault
                                                , str
                                                , switch
                                                , value
                                                )
import           Rational                       ( parseRat
                                                , ratPp
                                                )
import           System.IO                      ( stdin )




instance (Show i, Integral i) => ForthType (Ratio i) where
  tyShow    = ratPp
  tyToInt   = Just . floor
  tyFromInt = fromIntegral
  tyFromBool t = if t then -1 else 0

-- * Command line flags

data Options = Options
  { allowRecursive   :: Bool
  , tracelevel       :: Int
  , includeLibraries :: [FilePath]
  }

options :: ParserInfo Options
options = info
  (    Options
  <$>  switch
         (long "recursive" <> short 'r' <> help
           "Allow recursive word definitions"
         )
  <*>  option
         auto
         (  long "tracing"
         <> short 't'
         <> help "tracing level (-1 to 3)"
         <> showDefault
         <> value 1
         <> metavar "INT"
         )
  <*>  many (argument str (metavar "[FORTH SOURCE FILENAMES...]"))
  <**> helper
  )
  (fullDesc <> progDesc "A rudimentary Forth repl written in Haskell" <> header
    "H-FORTH"
  )

main :: IO ()
main = do
  opts <- execParser options
  sig  <- newMVar False
  let args = includeLibraries opts
  let d :: Dict () Rational []
      d  = coreDict
      vm = (emptyVm () parseRat sig)
        { dict      = d
        , inputPort = Just stdin
        , tracing   = tracelevel opts
        , recursive = if allowRecursive opts then fwUndefined else Nothing
        , memory = Just $ replicate 1000 0
        }
      initF = loadFiles args
  repl vm initF
