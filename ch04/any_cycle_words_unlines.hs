import Prelude hiding (any, cycle, words, unlines)

any :: (a -> Bool) -> [a] -> Bool
any p xs = foldr step False xs
    where step x b | p x || b  = True
                   | otherwise = False

cycle :: [a] -> [a]
cycle xs = foldr step [] [xs]
    where step xs ys = xs ++ (step xs ys)

isWhiteSpace :: Char -> Bool
isWhiteSpace ' ' = True
isWhiteSpace '\t' = True
isWhiteSpace '\n' = True
isWhiteSpace _ = False

words :: String -> [String]
words "" = []
words xs = foldr step [[]] xs
    where step x (a:as) | isWhiteSpace x = if null a then (a:as) else  []:a:as
                        | otherwise = (x:a):as

unlines :: [String] -> String
unlines [] = ""
unlines xs = foldr step "" xs
    where step x acc = x ++ ('\n':acc)
