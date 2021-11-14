module HW2.T1
  ( Option (..),
    Pair (..),
    Quad (..),
    Annotated (..),
    Except (..),
    Prioritised (..),
    Stream (..),
    List (..),
    Fun (..),
    Tree (..),
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

data Option a = None | Some a

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption f = g
  where
    g None = None
    g (Some a) = Some (f a)

data Pair a = P a a

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P a1 a2) = P (f a1) (f a2)

data Quad a = Q a a a a

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q a1 a2 a3 a4) = Q (f a1) (f a2) (f a3) (f a4)

data Annotated e a = a :# e deriving (Show)

infix 0 :#

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = f a :# e

data Except e a = Error e | Success a deriving (Show)

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept f (Success a) = Success (f a)
mapExcept _ (Error e) = Error e

data Prioritised a = Low a | Medium a | High a

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low a) = Low (f a)
mapPrioritised f (Medium a) = Medium (f a)
mapPrioritised f (High a) = High (f a)

data Stream a = a :> Stream a

infixr 5 :>

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (a :> as) = f a :> mapStream f as

data List a = Nil | a :. List a

infixr 5 :.

mapList :: (a -> b) -> (List a -> List b)
mapList f (a :. as) = f a :. mapList f as
mapList _ _ = Nil

data Fun i a = F (i -> a)

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F fun) = F (f . fun)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree f (Branch l a r) = Branch (mapTree f l) (f a) (mapTree f r)
mapTree _ _ = Leaf
