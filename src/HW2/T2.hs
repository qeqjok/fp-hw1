module HW2.T2
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

    -- * Dist functions
    distOption,
    distPair,
    distQuad,
    distAnnotated,
    distExcept,
    distPrioritised,
    distStream,
    distList,
    distFun,

    -- * Wrap functions
    wrapOption,
    wrapPair,
    wrapQuad,
    wrapAnnotated,
    wrapExcept,
    wrapPrioritised,
    wrapStream,
    wrapList,
    wrapFun,
  )
where

import HW2.T1 (Annotated (..), Except (..), Fun (..), List (..), Option (..), Pair (..),
               Prioritised (..), Quad (..), Stream (..))

-- | Bisequence for tuple and `Option`.
distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _)        = None
distOption (_, None)        = None
distOption (Some l, Some r) = Some (l, r)

-- | Bisequence for tuple and `Pair`.
distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P l1 l2, P r1 r2) = P (l1, r1) (l2, r2)

-- | Bisequence for tuple and `Quad`.
distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q l1 l2 l3 l4, Q r1 r2 r3 r4) = Q (l1, r1) (l2, r2) (l3, r3) (l4, r4)

-- | Bisequence for tuple and `Annotated`.
distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (l :# le, r :# re) = (l, r) :# (le <> re)

-- | Bisequence for tuple and `Except`.
distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _)           = Error e
distExcept (_, Error e)           = Error e
distExcept (Success l, Success r) = Success (l, r)

-- | Function for extract value from `Prioritised`.
extractPrioritised :: Prioritised a -> a
extractPrioritised (High a)   = a
extractPrioritised (Medium a) = a
extractPrioritised (Low a)    = a

-- | Bisequence for tuple and `Prioritised`.
distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High l, r)    = High (l, extractPrioritised r)
distPrioritised (l, High r)    = High (extractPrioritised l, r)
distPrioritised (Medium l, r)  = Medium (l, extractPrioritised r)
distPrioritised (l, Medium r)  = Medium (extractPrioritised l, r)
distPrioritised (Low l, Low r) = Low (l, r)

-- | Bisequence for tuple and `Stream`.
distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (l :> ls, r :> rs) = (l, r) :> distStream (ls, rs)

-- | Bisequence for tuple and `List`.
distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (a, b) = g a b b
  where
    g Nil _ _                = Nil
    g (_ :. ls) Nil ss       = g ls ss ss
    g (l :. ls) (r :. rs) ss = (l, r) :. g (l :. ls) rs ss

-- | Bisequence for tuple and `Fun`.
distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F l, F r) = F (\i -> (l i, r i))

-- | Wrap value into `Option`.
wrapOption :: a -> Option a
wrapOption = Some

-- | Wrap value into `Pair`.
wrapPair :: a -> Pair a
wrapPair x = P x x

-- | Wrap value into `Quad`.
wrapQuad :: a -> Quad a
wrapQuad x = Q x x x x

-- | Wrap value into `Annotated`.
wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated x = x :# mempty

-- | Wrap value into `Except`.
wrapExcept :: a -> Except e a
wrapExcept = Success

-- | Wrap value into `Prioritised`.
wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

-- | Wrap value into `Stream`.
wrapStream :: a -> Stream a
wrapStream x = x :> wrapStream x

-- | Wrap value into `List`.
wrapList :: a -> List a
wrapList x = x :. Nil

-- | Wrap value into `Fun`.
wrapFun :: a -> Fun i a
wrapFun x = F (const x)
