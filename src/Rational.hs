-- | Rational
module Rational where

import Data.Ratio {- base -}
import Safe {- safe -}
import qualified Text.Read as R {- base -}

sep :: Eq a => a -> [a] -> ([a],[a])
sep c s = let (lhs,rhs) = break (== c) s in (lhs,tailDef [] rhs)

bimap1 :: (t -> t1) -> (t, t) -> (t1, t1)
bimap1 f (p,q) = (f p,f q)

parse_int :: String -> Maybe Integer
parse_int = R.readMaybe

parse_rat :: String -> Maybe Rational
parse_rat s =
    case bimap1 parse_int (sep '/' s) of
      (Just n,Just d) -> Just (n % d)
      _ ->
          case parse_int s of
            Just i -> Just (fromInteger i)
            Nothing -> fmap realToFrac (R.readMaybe s :: Maybe Double)

rat_pp :: (Show i,Integral i) => Ratio i -> String
rat_pp r =
    let n = numerator r
        d = denominator r
    in if d == 1 then show n else concat [show n,"/",show d]


