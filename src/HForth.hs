{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HForth where

import           Data.Semigroup

-- data Value = Fail String | I Int | F Double | S String | B Bool | L [Value] | P (Value, Value) deriving (Show, Eq, Ord, Num)
type Value = Int

-- A simple stack machine

type CodeItem = Dstack -> Code -> Rstack -> Dstack

data Code = Empty | CodeItem :> Code
infixr 5 :>

instance Semigroup Code where
  Empty     <> ys = ys
  (x :> xs) <> ys = x :> (xs <> ys)
  stimes = stimesMonoid

instance Monoid Code where
  mempty = Empty


(+>) :: Code -> Code -> Code
(+>) = mappend


lcode :: [CodeItem] -> Code
lcode = foldr (:>) Empty

type Dstack = [Value]

type Rstack = [Code]

stop :: CodeItem
stop stk _ _ = stk

endprog :: Code
endprog = stop :> Empty

start :: Code -> Dstack
start Empty           = []
start (instr :> prog) = instr [] prog [endprog]

-- Operators

add :: CodeItem
add = op2 (+) -- This (+) acts on Values

sub :: CodeItem
sub = op2 (-)

mul :: CodeItem
mul = op2 (*)

op2 :: (Value -> Value -> Value) -> CodeItem
op2 op = stkop (\(x : y : stk) -> y `op` x : stk)

stkop :: (Dstack -> Dstack) -> CodeItem
stkop op stk (instr :> code) rstack = instr (op stk) code rstack


relop :: (Value -> Value -> Bool) -> CodeItem
relop op = stkop (\(x : y : stk) -> (if y `op` x then -1 else 0) : stk)

eq :: CodeItem
eq = relop (==)

gt :: CodeItem
gt = relop (>) -- etc.

ld :: Value -> CodeItem
ld c = stkop (c :) -- Load a known constant

tld :: (t -> Value) -> t -> CodeItem
tld t c = ld (t c) -- ... a specific constructor

ild :: Int -> CodeItem
ild = ld  -- e.g., Integer

--fld :: Double -> CodeItem
--fld = tld F -- Double, etc.

dup :: CodeItem
dup = stkop (\s@(x : _) -> x : s) -- Classical FORTH/PS stack ops

pop :: CodeItem
pop = stkop (\(_ : stk) -> stk) -- (or drop.)

exch :: CodeItem
exch = stkop (\(x : y : stk) -> y : x : stk) -- (or swap.)

roll :: Int -> CodeItem
roll n = stkop rollp
  where rollp (x : l) = let (t, d) = splitAt n l in t ++ (x : d)

call :: Code -> CodeItem
call (instr :> prc) stk cod rstk = instr stk prc (cod : rstk)

jmp :: Code -> CodeItem
jmp (instr :> proc) stk _ = instr stk proc -- Yes, the ‘goto’. . .

ret :: CodeItem
ret stk _ ((instr :> code) : rstk) = instr stk code rstk

ifjmp :: Code -> CodeItem
ifjmp (instr :> ccode) (cond : stk) (nxt :> ncode) | cond /= 0 = instr stk ccode
                                                   | otherwise = nxt stk ncode

ifNjmp :: Code -> CodeItem
ifNjmp (instr :> ccode) (cond : stk) (nxt :> ncode)
  | cond /= 0 = nxt stk ncode
  | otherwise = instr stk ccode

ifelse_gen :: Code -> Code -> Code -> Code -> Code
ifelse_gen cond thcode elcode continue =
  let othwise = elcode +> continue
  in  cond +> (ifNjmp othwise :> thcode) +> (jmp continue :> othwise)


while_gen :: Code -> Code -> Code -> Code
while_gen cond block continue =
  let wchunk = cond +> (ifNjmp continue :> block) +> (jmp wchunk :> continue)
  in  wchunk

param :: Int -> CodeItem
param n = stkop (\stk -> stk !! n : stk)

trap :: (CodeItem -> Code) -> Code -> CodeItem
trap block (instr :> escode) stk code rstk =
  let throw _ _ _ = instr stk (escode +> endprog) rstk
  in  jmp (block throw +> code) stk code rstk


-- Examples

factorial :: Code
factorial = ifelse_gen initf tproc eproc (ret :> Empty)
 where
  initf = lcode [dup, ild 0, eq]
  tproc = lcode [pop, ild 1]
  eproc = lcode [dup, ild 1, sub, call factorial, mul]

ftest :: Code
ftest = lcode [ild 6, call factorial, ret]

itfact :: Code
itfact = init +> while_gen (ild 0 :> gt :> Empty) loop fin
 where
  init = lcode [dup, ild 1, exch]
  loop = lcode [param 1, mul, exch, ild 1, sub, exch, param 1]
  fin  = lcode [exch, pop, ret]

itftest :: Code
itftest = lcode [ild 7, jmp itfact] -- note the abbrev. of call/ret

