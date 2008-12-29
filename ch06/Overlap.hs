{-# LANGUAGE FlexibleInstances #-}

class Foo a where
    foo :: a -> String

instance Foo Int where
    foo = show

instance Foo (Int, Int) where
    foo (a, b) = foo a ++ ", " ++ foo b

instance (Foo a, Foo b) => Foo (a, b) where
    foo (a, b) = ">>" ++ foo a ++ " " ++ foo b ++ "<<"
