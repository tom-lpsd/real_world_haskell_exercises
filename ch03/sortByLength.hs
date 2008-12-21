
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [l | l <- xs, l < x] ++
               [x] ++
               qsort [r | r <- xs, r >= x]

-- belows art not sophisticated. 
bsort :: (Ord a) => [a] -> [a]
bsort [] = []
bsort (x:xs) = let (m, zs) = bubbleup x xs in m:bsort zs
    where
      bubbleup x [] = (x, [])
      bubbleup x (y:ys)
          | x > y = let (m, zs) = bubbleup y ys in (m, x:zs)
          | otherwise = let (m, zs) = bubbleup x ys in (m, y:zs)

sortByLength :: [[a]] -> [[a]]
sortByLength [] = []
sortByLength (x:xs) = let (m, zs) = bubbleup x xs in m:sortByLength zs
    where
      bubbleup x [] = (x, [])
      bubbleup x (y:ys)
          | length x > length y = let (m, zs) = bubbleup y ys in (m, x:zs)
          | otherwise = let (m, zs) = bubbleup x ys in (m, y:zs)
