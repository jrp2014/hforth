module Words where

import HForth

fwOver = pop' >>= \p -> pop' >>= \q -> push' q >> push' p >> push' q
fwRot =
  pop' >>= \p -> pop' >>= \q -> pop' >>= \r -> push' q >> push' p >> push' r
fw2Dup = pop' >>= \p -> pop' >>= \q -> push' q >> push' p >> push' q >> push' p


fwQDup :: (Eq a, ForthType a) => ForthStep w a
fwQDup = forthBlock [fwDup, fwDup, fwQExit, fwDrop]


-- | roll
fwRoll :: (Eq a, Num a, ForthType a) => ForthStep w a
--fwRoll = fwQDup >> fw0EQ >> interpretIf
--  (next, fwSwap >> fwGTR >> fw1Minus >> fwRoll >> fwRGT >> fwSwap)
fwRoll = forthBlock
  [fwQDup, fw0EQ, fwQExit, fwSwap, fwGTR, fw1Minus, fwRoll, fwRGT, fwSwap]


-- | 0=
fw0EQ :: (Eq a, ForthType a) => ForthStep w a
fw0EQ = predicateOp (== tyFromInt 0)

-- | 0<
fw0LT :: (Ord a, ForthType a) => ForthStep w a
fw0LT = predicateOp (< tyFromInt 0)

-- | 1+
fw1Plus :: (Num a, ForthType a) => ForthStep w a
fw1Plus = unaryOp (\x -> x + tyFromInt 1)

-- | 1-
fw1Minus :: (Num a, ForthType a) => ForthStep w a
fw1Minus = unaryOp (\x -> x - tyFromInt 1)

-- | ( xu ... x1 x0 u -- xu ... x1 x0 xu )
fwPick :: ForthType a => ForthStep w a
fwPick = do
  vm <- getVm
  case stack vm of
    DC n : s' ->
      let n' = tyToInt' "PICK" n
          e  = s' !! n' -- unsafe
      in  put vm { stack = e : s' } >> next
    _ -> throwError "PICK"

-- | nip
fwNip :: ForthStep w a
fwNip = fwSwap >> fwDrop

-- | +
fwPlus :: (Num a) => ForthStep w a
fwPlus = binaryOp (+)
-- | -
fwMinus :: (Num a) => ForthStep w a
fwMinus = binaryOp (-)

-- | <
fwEQ :: (Ord a, ForthType a) => ForthStep w a
fwEQ = comparisonOp (==)

-- | <
fwLT :: (Ord a, ForthType a) => ForthStep w a
fwLT = comparisonOp (<)

-- | <
fwGT :: (Ord a, ForthType a) => ForthStep w a
fwGT = comparisonOp (>)

-- (/mod
fwOPSlashMod :: (Ord a, Num a, ForthType a) => ForthStep w a
fwOPSlashMod = forthBlock
  [ fwGTR
  , fwOver
  , fwOver
  , fwLT
  , fwRGT
  , fwSwap
  , fwQExit
  , fwGTR
  , fwSwap
  , fwOver
  , fwMinus
  , fwSwap
  , fwRGT
  , fw1Plus
  , fwOPSlashMod
  ]

-- | 10*
fw10Times :: (Num a, ForthType a) => ForthStep w a
fw10Times = unaryOp (* tyFromInt 10)

-- | (10u/mod
fwOP10uSlashMod :: (Ord a, Num a, ForthType a) => ForthStep w a
fwOP10uSlashMod = forthBlock
  [ push 2
  , fwPick
  , fwOver
  , fwGT
  , fw0EQ
  , fwQExit
  , fwDup
  , fwGTR
  , fw10Times
  , fwOP10uSlashMod
  , fwSwap
  , fwGTR
  , push 0
  , fwOPSlashMod
  , fwNip
  , fwRGT
  , fw10Times
  , fwPlus
  , fwRGT
  ]

-- | 10u/mod
fw10uSlashMod :: (Ord a, Num a, ForthType a) => ForthStep w a
fw10uSlashMod = forthBlock [push 0, push 1, fwOP10uSlashMod, fwDrop]

-- | (u.
fwOPuDot :: (Ord a, Num a, ForthType a) => ForthStep w a
fwOPuDot = forthBlock
  [ fwQDup
  , fw0EQ
  , fwQExit
  , fw10uSlashMod
  , fwOPuDot
  , push (tyFromInt $ ord '0')
  , fwPlus
  , fwEmit
  ]

