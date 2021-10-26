module HW1.T5
  ( -- * Split/Join functions
    joinWith,
    splitOn,
  ) where

import Data.List.NonEmpty (NonEmpty (..))

-- | Splits a list into sublists by a separator.
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr split ([] :| [])
  where
    split x (y :| ys)
      | sep == x = [] :| (y : ys)
      | otherwise = (x : y) :| ys

-- | Join a lists into list by a separator.
joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep = foldl1 join
  where
    join acc = (++) (acc ++ [sep])
