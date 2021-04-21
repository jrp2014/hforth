import           Control.Concurrent             ( newMVar ) {- base -}
import qualified Data.Map                      as M   {- containers -}
import           Data.Ratio                     ( Ratio ) {- base -}
import           System.Environment
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdin
                                                , stdout
                                                ) {- base -}

import           HForth                         ( Dict
                                                , Forth
                                                , ForthType(..)
                                                , VM(..)
                                                , coreDict
                                                , emptyVm
                                                , loadFiles
                                                , pop
                                                , push
                                                , repl
                                                )
import           Rational                       ( parseRat
                                                , ratPp
                                                )

-- * Primitives

instance (Show i,Integral i) => ForthType (Ratio i) where
  tyShow    = ratPp
  tyToInt   = Just . floor
  tyFromInt = fromIntegral
  tyFromBool t = if t then -1 else 0

{-
-- | Unary stack operation.
unaryOp :: (a -> a) -> Forth w a ()
unaryOp f = pop >>= push . f
-}

binaryOp'' :: (i -> a) -> (a -> i) -> (a -> a -> a) -> Forth w i (Either () ())
binaryOp'' f g h = pop >>= \y -> pop >>= \x -> push (g (h (f x) (f y)))

binaryOp' :: (Integer -> Integer -> Integer) -> Forth w Rational (Either () ())
binaryOp' = binaryOp'' floor fromInteger

-- | Binary stack operation.  The first value on the stack is the RHS.
binaryOp :: (a -> a -> a) -> Forth w a (Either () ())
binaryOp f = pop >>= \y -> pop >>= \x -> push (f x y)

-- | 'binaryOp', /rep/ translates the result so it can be placed onto the stack.
comparisonOp :: ForthType a => (a -> a -> Bool) -> Forth w a (Either () ())
comparisonOp f = binaryOp (\x y -> tyFromBool (f x y))

-- | Forth word @/mod@.
fwDivMod :: Forth w Rational (Either () ())
fwDivMod = pop >>= \p -> pop >>= \q ->
  let (r, s) = floor q `divMod` floor p
  in  push (fromInteger s) >> push (fromInteger r)

ratDict :: Dict w Rational
ratDict = M.fromList
  [ ("+"      , binaryOp (+))
  , ("*"      , binaryOp (*))
  , ("-"      , binaryOp (-))
     -- FRACTIONAL
  , ("/"      , binaryOp (/))
     -- INTEGRAL
  , ("mod"    , binaryOp' mod)
  , ("div"    , binaryOp' div)
  , ("div-mod", fwDivMod)
    -- EQ
  , ("="      , comparisonOp (==))
    -- ORD
  , ("<"      , comparisonOp (<))
  , ("<="     , comparisonOp (<=))
  , (">"      , comparisonOp (>))
  , (">="     , comparisonOp (>=))
  ]

main :: IO ()
main = do
  sig <- newMVar False
  args <- getArgs -- TODO :: put in propoer command line argument handling
  let
    d :: Dict () Rational
    d  = coreDict
    --d     = M.unions [coreDict, ratDict]
    vm = (emptyVm () parseRat sig) { dict      = d
                                   , inputPort = Just stdin
                                   , tracing   = 1
                                   }
    initF = loadFiles args -- loadFiles ["stdlib.fs", "ratlib.fs"]
  putStrLn "RAT-FORTH"
--  hSetBuffering stdout NoBuffering
  repl vm initF
