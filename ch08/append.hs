append :: [a] -> [a] -> [a]
append (x:xs) ys = x `seq` x : (xs ++ ys)
append []     ys = ys

tappend :: [a] -> [a] -> [a]
tappend xs ys = tappend' xs ys []
    where tappend' (x:xs) ys acc = x `seq` tappend' xs ys (x:acc)
          tappend' [] (y:ys) acc = y `seq` tappend' [] ys (y:acc)
          tappend' [] []     acc = reverse acc

-- try these
-- take 2 $ append  [1, 2, 3 `div` 0] [4, 5, 6] -- => [1,2]
-- take 2 $ tappend [1, 2, 3 `div` 0] [4, 5, 6] -- => Exception: divide by zero
