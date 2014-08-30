-- from  http://article.gmane.org/gmane.comp.lang.haskell.libraries/13444

module TestMap where

import Data.Map as M
import Random as R

-- Given a tree size, returns Just n if n deletions caused an
-- unbalanced tree, or Maybe if the tree remained valid.  The IO is so
-- that we can get random numbers.  Sorry.
test_map :: Int -> IO (Maybe Int)
test_map size =
    do map <- random_map size
       return (check_map map)

random_map :: Int -> IO (M.Map Int Int)
random_map size = loop 0 M.empty
  where loop i map
            | i >= size = return map
            | otherwise =
                do let i' = i + 1
                   k <- R.randomRIO (0, i')
                   loop i' (M.insert k k map)

check_map :: (Ord k) => M.Map k v -> Maybe Int
check_map map = loop 0 map
  where loop i map
            | not (M.valid map) = Just i
            | M.null map = Nothing
            | otherwise = loop (i + 1) (deleteMin map)
