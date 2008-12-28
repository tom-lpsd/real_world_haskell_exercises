{-# LANGUAGE TypeSynonymInstances #-}

class Borked a where
    bork :: a -> String

instance Borked Int where
    bork = show

instance Borked String where
    bork = id

instance Borked (Int, Int) where
    bork (a, b) = bork a ++ ", " ++ bork b
