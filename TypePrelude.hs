{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module TypePrelude where

import Data.Kind
import GHC.TypeLits

type family MyDouble (x :: Nat) :: Nat where
  MyDouble x = x + x

type family Not (x :: Bool) :: Bool where
  Not True  = False
  Not False = True

type family Even (n :: Nat) :: Bool where
  Even 0 = True
  Even 1 = False
  Even n = Odd (n-1)

type family Odd (n :: Nat) :: Bool where
  Odd 0 = False
  Odd 1 = True
  Odd n = Even (n-1)

type family Length (n :: [Type]) :: Nat where
  Length '[] = 0
  Length (_':xs) = 1 + Length xs

type family Fst (p :: (Type, Type)) :: Type where
  Fst '(a, _) = a

type family Snd (p :: (Type, Type)) :: Type where
  Snd '(_, b) = b

type family Head (xs :: [Type]) :: Type where
  Head (x:_) = x

type family Tail (xs :: [Type]) :: [Type] where
  Tail (_:xs) = xs

type family Take (n :: Nat) (xs :: [Type]) :: [Type] where
  Take _ '[] = '[]
  Take 0 _ = '[]
  Take n (x ': xs) = x ': Take (n-1) xs

type family Drop (n :: Nat) (xs :: [Type]) :: [Type] where
  Drop _ '[] = '[]
  Drop 0 xs = xs
  Drop n (x':xs) = Drop (n-1) xs

type family Zip (xs :: [Type]) (ys :: [Type]) :: [(Type, Type)] where
  Zip _ '[] = '[]
  Zip '[] _ = '[]
  Zip (x ': xs) (y ': ys) = '(x, y) ': Zip xs ys

type family Id (a :: Type) :: Type where
  Id a = a