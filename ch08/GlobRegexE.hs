module GlobRegex
    (
      globToRegex
    , matchesGlob
    ) where

import Text.Regex.Posix ((=~))

type GlobError = String

globToRegex :: String -> Either GlobError String
globToRegex cs = case globToRegex' cs of
                   Right r -> Right ('^' : r ++ "$")
                   Left  e -> Left e

globToRegex' :: String -> Either GlobError String
globToRegex' "" = Right ""

globToRegex' ('*':cs) = case globToRegex' cs of
                          Right r -> Right (".*" ++ r)
                          Left  e -> Left e

globToRegex' ('?':cs) = case globToRegex' cs of
                          Right r -> Right ('.' : r)
                          Left  e -> Left e

globToRegex' ('[':'!':c:cs) = case charClass cs of
                                Right r -> Right ("[^" ++ c:r)
                                Left  e -> Left e
globToRegex' ('[':c:cs)     = case charClass cs of
                                Right r -> Right ('[' :  c : r)
                                Left  e -> Left e
globToRegex' ('[':_)        = Left "unterminated character class"

globToRegex' (c:cs) = case globToRegex' cs of
                        Right r -> Right (escape c ++ r)
                        Left  e -> Left e

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"

charClass :: String -> Either GlobError String
charClass (']':cs) = case globToRegex' cs of
                       Right r -> Right (']' : r)
                       Left  e -> Left e
charClass (c:cs)   = case charClass cs of
                       Right r -> Right (c : r)
                       Left  e -> Left e
charClass []       = Left "unterminated character class"

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = case globToRegex pat of
                           Right regexPattern -> name =~ regexPattern
                           Left  e -> False
