import Control.Concurrent {- base -}
import qualified Data.Map as M {- containers -}
import Data.Ratio {- base -}
import System.IO {- base -}

import HForth
import Rational

-- * Primitives

instance (Show i,Integral i) => Forth_Type (Ratio i) where
    ty_show = rat_pp
    ty_to_int = Just . floor
    ty_from_int = fromIntegral
    ty_from_bool t = if t then -1 else 0

{-
-- | Unary stack operation.
unary_op :: (a -> a) -> Forth w a ()
unary_op f = pop >>= push . f
-}

binary_op'' :: (i -> a) -> (a -> i) -> (a -> a -> a) -> Forth w i ()
binary_op'' f g h = pop >>= \y -> pop >>= \x -> push (g (h (f x) (f y)))

binary_op' :: (Integer -> Integer -> Integer) -> Forth w Rational ()
binary_op' = binary_op'' floor fromInteger

-- | Binary stack operation.  The first value on the stack is the RHS.
binary_op :: (a -> a -> a) -> Forth w a ()
binary_op f = pop >>= \y -> pop >>= \x -> push (f x y)

-- | 'binary_op', /rep/ translates the result so it can be placed onto the stack.
comparison_op :: Forth_Type a => (a -> a -> Bool) -> Forth w a ()
comparison_op f = binary_op (\x y -> ty_from_bool (f x y))

-- | Forth word @/mod@.
fw_div_mod :: Forth w Rational ()
fw_div_mod =
    pop >>= \p -> pop >>= \q ->
    let (r,s) = floor q `divMod` floor p
    in push (fromInteger s) >> push (fromInteger r)

rat_dict :: Dict w Rational
rat_dict = M.fromList
    [("+",binary_op (+))
    ,("*",binary_op (*))
    ,("-",binary_op (-))
     -- FRACTIONAL
    ,("/",binary_op (/))
     -- INTEGRAL
    ,("mod",binary_op' mod)
    ,("div",binary_op' div)
    ,("div-mod",fw_div_mod)
    -- EQ
    ,("=",comparison_op (==))
    -- ORD
    ,("<",comparison_op (<))
    ,("<=",comparison_op (<=))
    ,(">",comparison_op (>))
    ,(">=",comparison_op (>=))]

main :: IO ()
main = do
  sig <- newMVar False
  let d :: Dict () Rational
      d = M.unions [core_dict,rat_dict]
      vm = (empty_vm () parse_rat sig) {dict = d, input_port = Just stdin}
      init_f = load_files ["stdlib.fs","ratlib.fs"]
  putStrLn "RAT-FORTH"
  repl vm init_f
