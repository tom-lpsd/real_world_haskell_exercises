module Sorting where

import Control.Parallel (par, pseq)

sort :: (Ord a) => [a] -> [a]
sort (x:xs) = lesser ++ x:greater
    where lesser  = sort [y | y <- xs, y <  x]
          greater = sort [y | y <- xs, y >= x]
sort _ = []

parSort :: (Ord a) => [a] -> [a]
parSort (x:xs) = force greater `par` (force lesser `pseq`
                                      (lesser ++ x:greater))
    where lesser  = parSort [y | y <- xs, y <  x]
          greater = parSort [y | y <- xs, y >= x]
parSort _ = []

parSort2 :: (Ord a) => Int -> [a] -> [a]
parSort2 d list@(x:xs)
    | d <= 0 = sort list
    | otherwise = force greater `par` (force lesser `pseq`
                                       (lesser ++ x:greater))
    where lesser  = parSort2 d' [y | y <- xs, y <  x]
          greater = parSort2 d' [y | y <- xs, y >= x]
          d' = d - 1
parSort2 _ _ = []

force :: [a] -> ()
force xs = go xs `pseq` ()
    where go (_:xs) = go xs
          go [] = 1
