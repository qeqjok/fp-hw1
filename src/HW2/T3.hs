module HW2.T3
  ( -- * The `Option` type
    Option (..),

    -- * The `Annotated` type
    Annotated (..),

    -- * The `Except` type
    Except (..),

    -- * The `List` type
    List (..),

    -- * The `Fun` type
    Fun (..),

    -- * Join functions
    joinOption,
    joinExcept,
    joinAnnotated,
    joinList,
    joinFun,
  )
where

import HW2.T1 (Annotated (..), Except (..), Fun (..), List (..), Option (..))

-- | Join for `Option`.
joinOption :: Option (Option a) -> Option a
joinOption (Some x) = x
joinOption _        = None

-- | Join for `Except`.
joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Success x) = x
joinExcept (Error e)   = Error e

-- | Join for `Annotated`.
joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((x :# e2) :# e1) = x :# (e1 <> e2)

-- | Join for `List`.
joinList :: List (List a) -> List a
joinList Nil               = Nil
joinList (Nil :. xs)       = joinList xs
joinList ((y :. ys) :. xs) = y :. joinList (ys :. xs)

-- | Join for `Fun`.
joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\i -> g i (f i))
  where
    g i (F h) = h i
