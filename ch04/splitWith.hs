splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs =
    let (e, r) = break p xs
    in e:splitWith p (tail r)
