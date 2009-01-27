{-# LANGUAGE FlexibleInstances, FunctionalDependencies,
             MultiParamTypeClasses #-}

module SupplyClass
    (
      MonadSupply(..)
    , S.Supply
    , S.runSupply
    ) where

import qualified Supply as S

class (Monad m) => MonadSupply s m where
    next :: m (Maybe s)

instance MonadSupply s (S.Supply s) where
    next = S.next
