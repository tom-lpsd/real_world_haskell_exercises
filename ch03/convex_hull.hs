import Prelude hiding (Either, Right, Left)

data Direction = Left | Right | Straight deriving Show

direction :: (Fractional a, Ord a) => (a, a) -> (a, a) -> (a, a) -> Direction
direction (x1, y1) (x2, y2) (x3, y3)
    | x2 == x1 = direction' (x1, y1) (x2, y2) (x3, y3)
    | y3 == bound = Straight
    | y3 >  bound = if (x2 - x1) > 0 then Left else Right
    | otherwise = if (x2 - x1) > 0 then Right else Left
    where
      grad  = (y2 - y1) / (x2 - x1)
      bias  = (y1 * x2 - y2 * x1) / (x2 - x1)
      bound = (x3 * grad + bias)
      -- case of (x1 == x2)
      direction' (x1, y1) (_, y2) (x3, _)
          | (x3 - x1) == 0 = Straight
          | (x3 - x1) * (y2 - y1) > 0 = Right
          | otherwise = Left

directions :: (Fractional a, Ord a) => [(a, a)] -> [Direction]
directions (x:y:z:[]) = [direction x y z]
directions (x:y:z:zs) = (direction x y z):directions (y:z:zs)

min_point :: (Fractional a, Ord a) => [(a, a)] -> ((a, a), [(a, a)])
min_point (x:[]) = (x, [])
min_point ((x, y):xs) = let ((mx, my), ys) = min_point xs
                        in if y < my
                           then ((x, y), (mx, my):ys)
                           else ((mx, my), (x, y):ys)

tangent :: (Fractional a, Ord a) => (a, a) -> (a, a) -> a
tangent (x1, y1) (x2, y2) = - (x2 - x1) / (y2 - y1)



drop_right [] y = y
drop_right (Right:xs) (y:ys) = drop_right xs ys
drop_right (_:xs) (y:ys) = y:(drop_right xs ys)

sortByAngle :: (Fractional a, Ord a) => (a, a) -> [(a, a)] -> [(a, a)]
sortByAngle c [] = []
sortByAngle c (x:xs) = let (m, zs) = bubbleup x xs in m:(sortByAngle c zs)
    where
      bubbleup x [] = (x, [])
      bubbleup x (y:ys)
          | (tangent c x) > (tangent c y) = let (m, zs) = bubbleup y ys in (m, x:zs)
          | otherwise = let (m, zs) = bubbleup x ys in (m, y:zs)

graham_scan xs = let (c, ys) = min_point xs
                     zs = sortByAngle c ys
                     ds = directions (c:zs)
                 in c:drop_right ds zs
