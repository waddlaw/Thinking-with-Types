{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module TypeQsort where

import Data.Kind (Type)
import GHC.TypeLits (Nat, CmpNat)

type family If (b :: Bool) (t :: [Nat]) (f :: [Nat]) :: [Nat] where
  If 'True  t _ = t
  If 'False _ f = f

type family (+++) (xs :: [Nat]) (ys :: [Nat]) :: [Nat] where
  '[] +++ ys       = ys
  (x ': xs) +++ ys = x ': (xs +++ ys)

-- type family Filter (p :: (Nat -> Bool)) (xs :: [Nat]) :: [Nat] where
--   Filter _ '[]       = '[]
--   Filter p (x ': xs) = If (p x) (x ': Filter p xs) (Filter p xs)

-- https://github.com/aische/typelevel-examples#quicksort
type family FilterLower (s :: Nat) (xs :: [Nat]) :: [Nat] where
  FilterLower s '[] = '[]
  FilterLower s (x ': xs) = Lower s x (CmpNat x s) xs

type family Lower (s :: Nat) (x :: Nat) (o :: Ordering) (xs :: [Nat]) :: [Nat] where
  Lower s x LT xs = x ': FilterLower s xs
  Lower s x _  xs = FilterLower s xs

type family FilterHigher (s :: Nat) (xs :: [Nat]) :: [Nat] where
  FilterHigher s '[] = '[]
  FilterHigher s (x ': xs) = Higher s x (CmpNat x s) xs

type family Higher (s :: Nat) (x :: Nat) (o :: Ordering) (xs :: [Nat]) :: [Nat] where
  Higher s x LT xs = FilterHigher s xs
  Higher s x _ xs = x ': FilterHigher s xs

type family Qsort (xs :: [Nat]) :: [Nat] where
  Qsort '[]       = '[]
  Qsort (x ': xs) = Qsort (FilterLower x xs) +++ '[x] +++ Qsort (FilterHigher x xs)

{-
λ> :k! Qsort '[9,2,4,0,1]
Qsort '[9,2,4,0,1] :: [Nat]
= '[0, 1, 2, 4, 9]
-}