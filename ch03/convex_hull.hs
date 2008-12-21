import Prelude hiding (Either, Right, Left, tan)

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
min_point (t:[]) = (t, [])
min_point (t@(x, y):xs) 
    | y < my || (y == my && x < mx) = (t, m:ys)
    | otherwise = (m, t:ys)
    where
      (m@(mx, my), ys) = min_point xs

tan :: (Fractional a, Ord a) => (a, a) -> (a, a) -> a
tan (x1, y1) (x2, y2) = - (x2 - x1) / (y2 - y1)

sortByAngle :: (Fractional a, Ord a) => (a, a) -> [(a, a)] -> [(a, a)]
sortByAngle c [] = []
sortByAngle c (x:xs) = let (m, zs) = min x xs in m:(sortByAngle c zs)
    where
      min x [] = (x, [])
      min x (y:ys)
          | (tan c x) > (tan c y) = let (m, zs) = min y ys in (m, x:zs)
          | otherwise = let (m, zs) = min x ys in (m, y:zs)

graham_scan xs = let (c, ys) = min_point xs
                     zs = sortByAngle c ys
                 in graham_scan' (c:zs)
    where
      graham_scan' (x:y:[]) = [x, y]
      graham_scan' (x:y:z:zs) =
          case d of
            Right -> graham_scan' (x:z:zs)
            _ -> x:graham_scan' (y:z:zs)
          where 
            d = direction x y z
            r = graham_scan'


example = graham_scan [(-1, 3), (0, 4), (1, 7), (2, 5), (2.7, 6), (3, 1), (3.2, 9), (3.5, 4.1), (4, 2), (10, 8)]
