module HW1.T4
  ( -- * Functions
    tfoldr,
    treeToList,
  ) where

import HW1.T3 (Tree (..))

-- | `foldr` implementation for `Three`.
tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ acc Leaf = acc
tfoldr f acc (Branch _ l e r) =
  tfoldr f (f e (tfoldr f acc r)) l

-- | Convert `Three` to `List`,  output list is sorted.
treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []
