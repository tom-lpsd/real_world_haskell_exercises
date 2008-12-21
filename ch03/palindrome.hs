-- using reverse
palindrome :: [a] -> [a]
palindrome [] = []
palindrome xs = xs ++ (reverse xs)

-- plain implementation (is not efficient)
palindrome' :: [a] -> [a]
palindrome' [] = []
palindrome' (x:xs) = x:palindrome' xs ++ [x]

-- predicate for palindrome
palindromep :: (Eq a) => [a] -> Bool
palindromep [] = True
palindromep (_:[]) = False
palindromep (x:xs)
    | x == last xs = palindromep (init xs)
    | otherwise = False
