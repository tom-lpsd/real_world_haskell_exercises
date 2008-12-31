module GlobRegexX
    (
      globToRegex
    , matchesGlob
    , Case(..)
    ) where

import Data.Char(toLower)
import Text.Regex.Posix ((=~))

data Case = CIgnore | CSensitive deriving Show

globToRegex :: Case -> String -> String
globToRegex CIgnore    cs = globToRegex CSensitive (map toLower cs)
globToRegex CSensitive cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' "" = ""

globToRegex' ('*':cs) = ".*" ++ globToRegex' cs

globToRegex' ('?':cs) = '.' : globToRegex' cs

globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs)     = '[' :   c : charClass cs
globToRegex' ('[':_)        = error "unterminated character class"

globToRegex' (c:cs) = escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"

matchesGlob :: Case -> FilePath -> String -> Bool
matchesGlob CIgnore    name pat = (map toLower name) =~ globToRegex CIgnore pat
matchesGlob CSensitive name pat = name =~ globToRegex CSensitive pat
