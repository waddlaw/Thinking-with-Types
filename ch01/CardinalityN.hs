{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardinality where

import GHC.TypeLits
import Data.Kind
import Data.Proxy
import Data.Void

type family Cardinality (n :: Nat) (a :: Type) :: Nat where
  Cardinality _ Void         = 0
  Cardinality _ ()           = 1
  Cardinality _ Bool         = 2
  Cardinality n (a, b)       = Cardinality n a * Cardinality n b
  Cardinality n (Maybe a)    = 1 + Cardinality n a
  Cardinality n (Either a b) = Cardinality n a + Cardinality n b
  Cardinality n (a -> b)     = Cardinality n b ^ Cardinality n a
  Cardinality 0 _            = 0
  Cardinality n [a]          = 1 + Cardinality (n-1) a * Cardinality (n-1) [a]

{-
λ natVal (Proxy @(Cardinality 4 [Bool]))
15

λ> natVal (Proxy @(Cardinality 0 (Either Bool (Bool, Maybe Bool) -> Bool)))
256
-}