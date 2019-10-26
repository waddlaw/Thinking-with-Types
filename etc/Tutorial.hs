{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tutorial where

import Data.Kind
import GHC.TypeLits (Symbol, symbolVal, KnownSymbol)
import Data.Proxy

data Unit = MkUnit
data IntAndChar = MkIntAndChar Int Char

theFirstOne = MkIntAndChar 3 'a'
theSecond   = MkIntAndChar (-3) 'b'

nothingA :: Maybe a
nothingA = Nothing

nothingInt :: Maybe Int
nothingInt = Nothing

nothingChar :: Maybe Char
nothingChar = Nothing

data HigherKinded f a
  = Bare a
  | Wrapped (f a)

data Void

-- data Zero
-- data Succ a

-- type One   = Succ Zero
-- type Two   = Succ One
-- type Three = Succ Two
-- type Four  = Succ (Succ (Succ (Succ Zero)))

-- こんな感じの型が定義できてしまう。
-- type InConsistent = Succ Bool

data Nat = Zero | Succ Nat

data IntBool a where
  Int  :: Int -> IntBool Int
  Bool :: Bool -> IntBool Bool

extractIntBool :: IntBool a -> a
extractIntBool (Int _)  = 0
extractIntBool (Bool b) = b

data Vector (n :: Nat) (a :: Type) where
  VNil  :: Vector 'Zero a
  VCons :: a -> Vector n a -> Vector ('Succ n) a

instance Show a => Show (Vector n a) where
  show VNil         = "VNil"
  show (VCons a as) = "VCons " ++ show a ++ " (" ++ show as ++ ")"

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ n) m = add n (Succ m)

type family Add n m where
  Add 'Zero n = n
  -- Add ('Succ n) m = Add n ('Succ m)
  Add ('Succ n) m = 'Succ (Add n m)

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil xs = xs
-- append (VCons a rest) xs = append rest (VCons a xs)
append (VCons a rest) xs = VCons a (append rest xs)

data HList xs where
  HNil :: HList '[]
  (:::) :: a -> HList as -> HList (a ': as)

infixr 6 :::

-- instance Show (HList xs) where
--   show HNil         = "HNil"
--   show (x ::: rest) = "_ ::: " ++ show rest

instance Show (HList '[]) where
  show HNil         = "HNil"

instance (Show (HList as), Show a) 
  => Show (HList (a ': as)) where
  show (a ::: rest) = 
      show a ++ " ::: " ++ show rest

newtype s >> a = Named a

data HRec xs where
  HEmpty :: HRec '[]
  HCons  :: (s >> a) -> HRec xs -> HRec (s >> a ': xs)

instance Show (HRec '[]) where
  show _ = "HEmpty"

instance (Show a, KnownSymbol s, Show (HRec xs)) 
  => Show (HRec (s >> a ': xs)) where
  show (HCons (Named a) rest) =
      let val = show a
          key = symbolVal (Proxy :: Proxy s)
          more = show rest
       in "(" ++ key ++ ": " ++ val ++ ") " ++ more
