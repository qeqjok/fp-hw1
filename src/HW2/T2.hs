module HW2.T2
  ( Option (..),
    Pair (..),
    Quad (..),
    Annotated (..),
    Except (..),
    Prioritised (..),
    Stream (..),
    List (..),
    Fun (..),
    distOption,
    distPair,
    distQuad,
    distAnnotated,
    distExcept,
    distPrioritised,
    distStream,
    distList,
    distFun,
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

import HW2.T1
  ( Annotated (..),
    Except (..),
    Fun (..),
    List (..),
    Option (..),
    Pair (..),
    Prioritised (..),
    Quad (..),
    Stream (..),
  )

distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _) = None
distOption (_, None) = None
distOption (Some l, Some r) = Some (l, r)

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P l1 l2, P r1 r2) = P (l1, r1) (l2, r2)

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q l1 l2 l3 l4, Q r1 r2 r3 r4) = Q (l1, r1) (l2, r2) (l3, r3) (l4, r4)

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (l :# le, r :# re) = (l, r) :# (le <> re)

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _) = Error e
distExcept (_, Error e) = Error e
distExcept (Success l, Success r) = Success (l, r)

extractPrioritised :: Prioritised a -> a
extractPrioritised (High a) = a
extractPrioritised (Medium a) = a
extractPrioritised (Low a) = a

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High l, r) = High (l, extractPrioritised r)
distPrioritised (l, High r) = High (extractPrioritised l, r)
distPrioritised (Medium l, r) = Medium (l, extractPrioritised r)
distPrioritised (l, Medium r) = Medium (extractPrioritised l, r)
distPrioritised (Low l, Low r) = Low (l, r)

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (l :> ls, r :> rs) = (l, r) :> distStream (ls, rs)

distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (a, b) = g a b b
  where
    g Nil _ _ = Nil
    g (_ :. ls) Nil ss = g ls ss ss
    g (l :. ls) (r :. rs) ss = (l, r) :. g (l :. ls) rs ss

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F l, F r) = F (\i -> (l i, r i))

wrapOption :: a -> Option a
wrapOption = Some

wrapPair :: a -> Pair a
wrapPair x = P x x

wrapQuad :: a -> Quad a
wrapQuad x = Q x x x x

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated x = x :# mempty

wrapExcept :: a -> Except e a
wrapExcept = Success

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = High

wrapStream :: a -> Stream a
wrapStream x = x :> wrapStream x

wrapList :: a -> List a
wrapList x = x :. Nil

wrapFun :: a -> Fun i a
wrapFun x = F (const x)
