{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Cardinality where

import Data.Proxy
import Data.Void

class Cardinality a where
  cardinality :: Proxy a -> Int

instance Cardinality Void where
  cardinality _ = 0

instance Cardinality () where
  cardinality _ = forallElems (Proxy @())

instance Cardinality Bool where
  cardinality _ = forallElems (Proxy @Bool)

instance (Cardinality a, Cardinality b) => Cardinality (Either a b) where
  cardinality _ = cardinality (Proxy @a) + cardinality (Proxy @b)

forallElems :: (Bounded a, Enum a) => Proxy a -> Int
forallElems (_:: Proxy a) = length ([minBound .. maxBound] :: [a])