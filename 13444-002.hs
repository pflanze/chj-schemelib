-- from  http://article.gmane.org/gmane.comp.lang.haskell.libraries/13444

module TestSet where

import Data.Set as S
import Random as R

-- Given a tree size, returns Just n if n deletions caused an
-- unbalanced tree, or Maybe if the tree remained valid.  The IO is so
-- that we can get random numbers.  Sorry.
test_set :: Int -> IO (Maybe Int)
test_set size =
    do set <- random_set size
       return (check_set set)

random_set :: Int -> IO (S.Set Int)
random_set size = loop 0 S.empty
  where loop i set
            | i >= size = return set
            | otherwise =
                do let i' = i + 1
                   k <- R.randomRIO (0, i')
                   loop i' (S.insert k set)

check_set :: (Ord a) => S.Set a -> Maybe Int
check_set set = loop 0 set
  where loop i set
            | not (S.valid set) = Just i
            | S.null set = Nothing
            | otherwise = loop (i + 1) (deleteMin set)
