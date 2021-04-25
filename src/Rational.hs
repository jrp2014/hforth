-- | Rational
module Rational (parseInt, parseRat, ratPp) where

import           Data.Ratio                     ( (%)
                                                , Ratio
                                                , denominator
                                                , numerator
                                                )  {- base -}
import           Safe                           ( tailDef )  {- safe -}
import qualified Text.Read                     as R {- base -}

sep :: Eq a => a -> [a] -> ([a], [a])
sep c s = let (lhs, rhs) = break (== c) s in (lhs, tailDef [] rhs)

bimap1 :: (t -> t1) -> (t, t) -> (t1, t1)
bimap1 f (p, q) = (f p, f q)

parseInt :: String -> Maybe Integer
parseInt = R.readMaybe


parseRat :: String -> Maybe Rational
parseRat ['\'', ch, '\''] = Just $ fromIntegral $ fromEnum ch -- allow 'x' to represent an Int
parseRat s                = case bimap1 parseInt (sep '/' s) of
  (Just n, Just d) -> Just (n % d)
  _                -> case parseInt s of
    Just i  -> Just (fromIntegral i)
    Nothing -> fmap realToFrac (R.readMaybe s :: Maybe Double)

ratPp :: (Show i, Integral i) => Ratio i -> String
ratPp r =
  let n = numerator r
      d = denominator r
  in  if d == 1 then show n else concat [show n, "/", show d]


