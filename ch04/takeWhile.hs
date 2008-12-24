import Prelude hiding (takeWhile)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p xs = helper [] xs
    where
      helper acc [] = reverse acc
      helper acc (x:xs)
          | p x = helper (x:acc) xs
          | otherwise = reverse acc

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x = x:(takeWhile' p xs)
    | otherwise = []

takeWhile_fold :: (a -> Bool) -> [a] -> [a]
takeWhile_fold p xs = foldr step [] xs
    where step x acc | p x = x:acc
                     | otherwise = []
