import Prelude hiding (length)

-- naive implementation
length :: (Num a) => [b] -> a
length [] = 0
length (_:xs) = 1 + length xs

-- using tail recursion
length' :: (Num a) => [b] -> a
length' xs = len 0 xs
    where
      len n [] = n
      len n (_:xs) = len (n+1) xs
