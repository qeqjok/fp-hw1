module HW2.T3
  ( Option (..),
    Annotated (..),
    Except (..),
    List (..),
    Fun (..),
    joinOption,
    joinExcept,
    joinAnnotated,
    joinList,
    joinFun,
  )
where

import HW2.T1
  ( Annotated (..),
    Except (..),
    Fun (..),
    List (..),
    Option (..),
  )

joinOption :: Option (Option a) -> Option a
joinOption (Some x) = x
joinOption _ = None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Success x) = x
joinExcept (Error e) = Error e

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((x :# e2) :# e1) = x :# (e1 <> e2)

joinList :: List (List a) -> List a
joinList Nil = Nil
joinList (Nil :. xs) = joinList xs
joinList ((y :. ys) :. xs) = y :. joinList (ys :. xs)

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\i -> g i (f i))
  where
    g i (F h) = h i
