import Prelude hiding (Either, Right, Left)

data Direction = Left | Right | Straight deriving Show

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
