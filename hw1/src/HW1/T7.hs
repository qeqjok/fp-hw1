module HW1.T7
  ( -- * The @DotString@ type
    DotString (..),

    -- * The @Fun@ type
    Fun (..),

    -- * The @Inclusive@ type
    Inclusive (..),

    -- * The @ListPlus@ type
    ListPlus (..),
  ) where

data ListPlus a = a :+ ListPlus a | Last a

infixr 5 :+

instance Semigroup (ListPlus a) where
  (Last x) <> ys  = x :+ ys
  (x :+ xs) <> ys = x :+ (xs <> ys)

data Inclusive a b = This a | That b | Both a b

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (This al) <> (This ar)       = This (al <> ar)
  (That bl) <> (That br)       = That (bl <> br)
  (Both al bl) <> (Both ar br) = Both (al <> ar) (bl <> br)
  (This a) <> (That b)         = Both a b
  (That b) <> (This a)         = Both a b
  (That b) <> (Both ar br)     = Both ar (b <> br)
  (This a) <> (Both ar br)     = Both (a <> ar) br
  (Both al bl) <> (That b)     = Both al (bl <> b)
  (Both al bl) <> (This a)     = Both (al <> a) bl

newtype DotString = DS String

instance Semigroup DotString where
  (DS []) <> (DS r) = DS r
  (DS l) <> (DS []) = DS l
  (DS l) <> (DS r)  = DS (l <> "." <> r)

instance Monoid DotString where
  mempty = DS []

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (F l) <> (F r) = F (r . l)

instance Monoid (Fun a) where
  mempty = F id
