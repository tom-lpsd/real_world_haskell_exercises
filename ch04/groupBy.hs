groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy p (x:xs) = reverse (foldl step [[x]] xs)
    where step (a:as) x | p (head a) x = (a ++ [x]):as
                        | otherwise = [x]:a:as
