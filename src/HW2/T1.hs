module HW2.T1
  ( -- * The `Option` type
    Option (..),

    -- * The `Pair` type
    Pair (..),

    -- * The `Quad` type
    Quad (..),

    -- * The `Annotated` type
    Annotated (..),

    -- * The `Except` type
    Except (..),

    -- * The `Prioritised` type
    Prioritised (..),

    -- * The `Stream` type
    Stream (..),

    -- * The `List` type
    List (..),

    -- * The `Fun` type
    Fun (..),

    -- * The `Tree` type
    Tree (..),

    -- * Functions
    mapOption,
    mapPair,
    mapQuad,
    mapAnnotated,
    mapExcept,
    mapPrioritised,
    mapStream,
    mapList,
    mapFun,
    mapTree,
  )
where

-- | The `Option` type
data Option a
  = None -- ^ Nothing
  | Some a -- ^ Just some value

-- | fmap for `Option`.
mapOption :: (a -> b) -> (Option a -> Option b)
mapOption f = g
  where
    g None     = None
    g (Some a) = Some (f a)

data Pair a = P a a

-- | fmap for `Pair`.
mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P a1 a2) = P (f a1) (f a2)

data Quad a = Q a a a a

-- | fmap for `Quad`.
mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q a1 a2 a3 a4) = Q (f a1) (f a2) (f a3) (f a4)

data Annotated e a = a :# e

infix 0 :#

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = f a :# e

-- | fmap for `Except`.
data Except e a
  = Error e
  | Success a

-- | fmap for `Except`.
mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept f (Success a) = Success (f a)
mapExcept _ (Error e)   = Error e

data Prioritised a
  = Low a
  | Medium a
  | High a

-- | fmap for `Prioritised`.
mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low a)    = Low (f a)
mapPrioritised f (Medium a) = Medium (f a)
mapPrioritised f (High a)   = High (f a)

data Stream a = a :> Stream a

infixr 5 :>

-- | fmap for `Stream`.
mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (a :> as) = f a :> mapStream f as

data List a = Nil | a :. List a

infixr 5 :.

-- | fmap for `List`.
mapList :: (a -> b) -> (List a -> List b)
mapList f (a :. as) = f a :. mapList f as
mapList _ _         = Nil

data Fun i a = F (i -> a)

-- | fmap for `Fun`.
mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F fun) = F (f . fun)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

-- | fmap for `Tree`.
mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree f (Branch l a r) = Branch (mapTree f l) (f a) (mapTree f r)
mapTree _ _              = Leaf
