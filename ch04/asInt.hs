import Data.Char (digitToInt, isDigit)

asInt_fold :: String -> Int
asInt_fold ""  = error "empty string"
asInt_fold ('-':xs) = (-1) * asInt_fold xs
asInt_fold xs = foldl step 0 xs
    where
      step acc x 
          | isDigit x = acc * 10 + (digitToInt x)
          | otherwise = error "non digit character"

type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either ""  = Left "empty string"
asInt_either ('-':xs) = 
    let abs = asInt_either xs
    in case abs of
         Right i   -> Right (-1 * i)
         otherwise -> abs
asInt_either xs = foldl step (Right 0) xs
    where
      step acc x
          | isDigit x = case acc of 
                          Right i   -> Right (i * 10 + (digitToInt x))
                          otherwise -> acc
          | otherwise = Left ("non-digit " ++ [x])
