module HW1.T3
  ( -- * The @Tree@ type
    Tree (..),

    -- * Functions for @Three@
    tdepth,
    tFromList,
    tinsert,
    tmember,
    tsize,
  ) where

data Tree a = Leaf | Branch Int (Tree a) a (Tree a)

-- | Size of the tree.
tsize :: Tree a -> Int
tsize Leaf                = 0
tsize (Branch size _ _ _) = size

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth Leaf             = 0
tdepth (Branch _ l _ r) = 1 + max (tdepth l) (tdepth r)

-- | Check if the element is in the tree.
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember e (Branch _ l inside r) =
  case compare e inside of
    GT -> tmember e r
    LT -> tmember e l
    EQ -> True

-- | Build a tree from a list.
tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf

-- | Insert an element into the tree.
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert e Leaf = Branch 1 Leaf e Leaf
tinsert e (Branch size l inside r) =
  case compare e inside of
    GT ->
      let right = tinsert e r
       in Branch (size + (tsize right - tsize r)) l inside right
    LT ->
      let left = tinsert e l
       in Branch (size + (tsize left - tsize l)) left inside r
    EQ -> Branch size l inside r
