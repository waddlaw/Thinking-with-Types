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
  Cardinality 0 _            = 0
  Cardinality n (a, b)       = Cardinality (n-1) a * Cardinality (n-1) b
  Cardinality n (Either a b) = Cardinality (n-1) a + Cardinality (n-1) b
  Cardinality n (a -> b)     = Cardinality (n-1) b ^ Cardinality (Cardinality (n-1) b - 1) a
  Cardinality n [a]          = 1 + Cardinality (n-1) a * Cardinality (n-1) [a]

{-
λ natVal (Proxy @(Cardinality 0 [Bool]))
0
λ natVal (Proxy @(Cardinality 1 [Bool]))
1
λ natVal (Proxy @(Cardinality 2 [Bool]))
3
λ natVal (Proxy @(Cardinality 3 [Bool]))
7
λ natVal (Proxy @(Cardinality 4 [Bool]))
15
-}