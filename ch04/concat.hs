import Prelude hiding (concat)

concat :: [[a]] -> [a]
concat = foldr (++) [] 
