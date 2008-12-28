module NewType where

data DataInt = D Int deriving (Eq, Ord, Show)

newtype NewTypeInt = N Int deriving (Eq, Ord, Show)
