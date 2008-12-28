{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module BrokenClass where

import JSONClass

instance (JSON a) => JSON [a] where
    toJValue = undefined
    fromJValue = undefined

instance (JSON a) => JSON [(String, a)] where
    toJValue = undefined
    fromJValue = undefined
