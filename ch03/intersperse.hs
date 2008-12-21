intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (x:[]) = x
intersperse sep (x:xs) = x ++ sep:intersperse sep xs
