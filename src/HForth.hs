{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HForth where

import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Semigroup
import GHC.IO.Handle

-- data Value = Fail String | I Int | F Double | S String | B Bool | L [Value] | P (Value, Value) deriving (Show, Eq, Ord, Num)
type Value = Int

-- A simple stack machine

data Mode = Immediate | Compile deriving (Show, Eq)

type CodeItem = Dstack -> Code -> Rstack -> Dictionary -> Mode -> IO Dstack

data Code = Empty | CodeItem :> Code

infixr 5 :>

instance Semigroup Code where
  Empty <> ys = ys
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

type Token = String

type Entry = (Token, Code)

type Dictionary = [Entry]

--

stop :: CodeItem
stop stk _ _ _ _ = return stk

endprog :: Code
endprog = stop :> Empty

start :: Code -> IO Dstack
start Empty = return []
start (instr :> prog) = instr [] prog [endprog] [] Immediate

-- Operators

add :: CodeItem
add = op2 (+) -- This (+) acts on Values

sub :: CodeItem
sub = op2 (-)

mul :: CodeItem
mul = op2 (*)

divide :: CodeItem
divide = op2 div

op2 :: (Value -> Value -> Value) -> CodeItem
op2 op = stkop (\(x : y : stk) -> y `op` x : stk)

stkop :: (Dstack -> Dstack) -> CodeItem
stkop op stk (instr :> code) rstack dict = instr (op stk) code rstack dict

relop :: (Value -> Value -> Bool) -> CodeItem
relop op = stkop (\(x : y : stk) -> (if y `op` x then 1 else 0) : stk)

eq :: CodeItem
eq = relop (==)

gt :: CodeItem
gt = relop (>) -- etc.

lt0 :: CodeItem
lt0 = stkop (\(x : stk) -> (if x < 0 then 1 else 0) : stk)

ld :: Value -> CodeItem
ld c = stkop (c :) -- Load a known constant

tld :: (t -> Value) -> t -> CodeItem
tld t c = ld (t c) -- ... a specific constructor

ild :: Int -> CodeItem
ild = ld -- e.g., Integer

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
  where
    rollp (x : l) = let (t, d) = splitAt n l in t ++ (x : d)

param :: Int -> CodeItem
param n = stkop (\stk -> stk !! n : stk)

trap :: (CodeItem -> Code) -> Code -> CodeItem
trap block (instr :> escode) stk code rstk =
  let throw _ _ _ = instr stk (escode +> endprog) rstk
   in jmp (block throw +> code) stk code rstk

-- Memory

fetch :: CodeItem
fetch = undefined

store :: CodeItem
store = undefined

-- IO

echo :: CodeItem
echo stk@(ch : _) (instr :> code) rstack dict mode =
  putChar (chr ch) >> instr stk code rstack dict mode

key :: CodeItem
key stk (instr :> code) rstack dict mode =
  getChar >>= (\ch -> instr (ord ch : stk) code rstack dict mode)

read_ :: CodeItem
read_ stk (instr :> code) rstk dict mode = do
  putStrLn "read_"
  mch <- skipW
  case mch of
    Nothing -> putStrLn "read_: EOF" >> return stk
    Just ch -> do
      wd <- readWord
      let word = ch : wd
      putStrLn $ "read_ing " ++ word
      case lookup word dict of
        Just instruction -> run stk (instruction +> (instr :> code)) rstk dict mode
        Nothing ->
          -- see if it's a number
          case reads word :: [(Int, String)] of
            [(n, "")] -> instr (n : stk) code rstk dict mode -- ?? compile v interpret
            _ -> putStrLn ("read_: unknown word '" ++ word ++ "'") >> return stk

-- | return first non-whitespace char
skipW :: IO (Maybe Char)
skipW = do
  eof <- isEOF
  if eof
    then return Nothing
    else getChar >>= (\ch -> if isSpace ch then skipW else return $ Just ch)

-- | read a string until the next whitespace, consuming the first char of that
-- whitespace.
readWord :: IO String
readWord = go ""
  where
    go acc = do
      eof <- isEOF
      if eof
        then return acc
        else getChar >>= (\ch -> if isSpace ch then return acc else go (acc ++ [ch]))

-- Execution

call :: Code -> CodeItem
call (instr :> prc) stk cod rstk dict mode = putStrLn "call" >> instr stk prc (cod : rstk) dict mode

-- | or @ret@
exit :: CodeItem
exit stk _ ((instr :> code) : rstk) = instr stk code rstk

jmp :: Code -> CodeItem
jmp (instr :> proc) stk _ = instr stk proc -- Yes, the ‘goto’. . .

ifjmp :: Code -> CodeItem
ifjmp (instr :> ccode) (cond : stk) (nxt :> ncode)
  | cond /= 0 = instr stk ccode
  | otherwise = nxt stk ncode

ifNjmp :: Code -> CodeItem
ifNjmp (instr :> ccode) (cond : stk) (nxt :> ncode)
  | cond /= 0 = nxt stk ncode
  | otherwise = instr stk ccode

ifelse_gen :: Code -> Code -> Code -> Code -> Code
ifelse_gen cond thcode elcode continue =
  let othwise = elcode +> continue
   in cond +> (ifNjmp othwise :> thcode) +> (jmp continue :> othwise)

while_gen :: Code -> Code -> Code -> Code
while_gen cond block continue =
  let wchunk = cond +> (ifNjmp continue :> block) +> (jmp wchunk :> continue)
   in wchunk

run :: CodeItem
run stk (instr :> code) rstk dict mode =
  putStrLn "run" >> instr stk code rstk dict mode

-- Immediate (compilation) Operations

define :: CodeItem
define stk (instr :> code) rstk dict _mode = do
  mch <- skipW
  case mch of
    Nothing -> putStrLn "define: EOF" >> return stk
    Just ch -> do
      wd <- readWord
      instr stk code rstk ((ch : wd, Empty) : dict) Compile
define stk Empty rstk dict _mode = putStrLn "define Empty" >> return stk

-- type CodeItem = Dstack -> Code -> Rstack -> Dictionary -> IO Dstack
compile :: CodeItem
compile stk (instr :> code) rstk dict mode = instr stk code rstk dict Compile

immediate :: CodeItem
immediate stk (instr :> code) rstk dict mode =
  instr stk code rstk dict Immediate

-- Stack Operation

pick :: CodeItem
pick = stkop pickp where pickp (x : l) = (l !! x) : l

-- Internal

-- used by read, when the input isnot a known word
pushint :: String -> CodeItem
pushint = undefined

r :: CodeItem
r stk (instr :> code) rstk dict mode = undefined

initDict :: Dictionary
initDict =
  [ (":", define :> Empty),
    --  , ("immediate", immediate :> Empty)
    ("compile", compile :> Empty), -- ??
    ("_read", read_ :> Empty),
    ("run", run :> Empty), -- ??
    ("@", fetch :> Empty),
    ("!", store :> Empty),
    ("-", sub :> Empty),
    ("*", mul :> Empty),
    ("/", divide :> Empty),
    ("<0", lt0 :> Empty),
    ("exit", exit :> Empty),
    ("echo", echo :> Empty),
    ("key", key :> Empty),
    ("_pick", pick :> Empty)
  ]

defWord :: String -> Code -> Dictionary -> Dictionary
defWord name code dict = (name, code) : dict

boot :: Code -> IO Dstack
boot (instr :> prog) = instr [] prog [endprog] initDict Immediate

mainloop = read_ :> run :> mainloop

main :: IO ()
main = do
  stk <- boot mainloop
  print stk

{- Examples

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

-}
