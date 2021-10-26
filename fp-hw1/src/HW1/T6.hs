module HW1.T6
  ( -- * Functions
    mcat,
    epart,
  ) where

import Data.Maybe (fromMaybe)

-- | The concatenation of all the elements of a list of Maybe.
mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap . fromMaybe $ mempty

-- | Partitions a list of 'Either' into tuple.
epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldr (either left right) (mempty, mempty)
  where
    left e (l, r) = (mappend e l, r)
    right e (l, r) = (l, mappend e r)
