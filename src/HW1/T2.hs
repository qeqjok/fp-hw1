module HW1.T2
  ( -- * The @N@ type
    N (..),

    -- * Num actions
    nplus,
    nmult,
    nsub,
    nFromNatural,
    nToNum,

    -- * Ord actions
    ncmp,

    -- * Integral actions
    ndiv,
    nmod,

    -- * Parity checking
    nEven,
    nOdd
  ) where

import GHC.Natural (Natural)

data N = Z | S N

-- | Addition.
nplus :: N -> N -> N
nplus x Z     = x
nplus Z x     = x
nplus x (S y) = S (nplus x y)

-- | Multiplication.
nmult :: N -> N -> N
nmult _ Z     = Z
nmult Z _     = Z
nmult x (S Z) = x
nmult (S Z) x = x
nmult x (S y) = nplus (nmult x y) x

-- | Subtraction (`Nothing` if result is negative).
nsub :: N -> N -> Maybe N
nsub Z Z         = Just Z
nsub Z _         = Nothing
nsub x Z         = Just x
nsub (S x) (S y) = nsub x y

-- | Comparison (without deriving Ord).
ncmp :: N -> N -> Ordering
ncmp Z Z         = EQ
ncmp Z _         = LT
ncmp _ Z         = GT
ncmp (S x) (S y) = ncmp x y

-- | Create `N` from `Natural`.
nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural x = S (nFromNatural (x - 1))

-- | Create `Num` type from `N`.
nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S x) = 1 + nToNum x

-- | If the `N` is even returns `True`, `False` otherwise.
nEven :: N -> Bool
nEven Z         = True
nEven (S (S x)) = nEven x
nEven _         = False

-- | If the `N` is odd returns `True`, `False` otherwise.
nOdd :: N -> Bool
nOdd = not . nEven

-- | Integer division.
ndiv :: N -> N -> N
ndiv _ Z = error "division by zero"
ndiv Z _ = Z
ndiv x y
  | ncmp x y == LT = Z
  | ncmp x y == EQ = S Z
  | otherwise = S (ndiv (npSub x y) y)

-- | Modulo operation.
nmod :: N -> N -> N
nmod Z _ = Z
nmod _ Z = Z
nmod x y
  | ncmp x y == LT = x
  | ncmp x y == EQ = Z
  | otherwise = nmod (npSub x y) y

-- | Helping fun - subtraction (`Z` if result is negative).
npSub :: N -> N -> N
npSub Z _         = Z
npSub x Z         = x
npSub (S x) (S y) = npSub x y
