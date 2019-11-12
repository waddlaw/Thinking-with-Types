{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Ch08 where

import Data.Coerce (Coercible(..), coerce)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Monoid (Sum(..), Product (..))

-- newtype ZipList a = ZipList { getZipList :: [a] }
-- newtype Sum a = Sum { getSum :: a }

slowSum :: [Int] -> Int
slowSum = getSum . mconcat . fmap Sum

fastSum :: [Int] -> Int
fastSum = getSum . mconcat . coerce

{-
λ> slowSum [1..10000000]
50000005000000
(2.23 secs, 2,416,474,736 bytes)

λ> fastSum [1..10000000]
50000005000000
(2.04 secs, 1,532,378,736 bytes)
-}

newtype Reverse a = Reverse { getReverse :: a }
  deriving (Eq, Show)

instance Ord a => Ord (Reverse a) where
  compare (Reverse a) (Reverse b) = compare b a

data BST v
  = Empty
  | Branch (BST v) v (BST v)

type role BST nominal