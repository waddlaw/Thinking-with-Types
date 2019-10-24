{-# LANGUAGE InstanceSigs #-}
module Ch03 where

import Data.Functor.Contravariant
import Data.Functor.Invariant

newtype T1 a = T1  (Int -> a)
instance Functor T1 where
  fmap :: (a -> b) -> T1 a -> T1 b
  fmap f (T1 g) = T1 (f . g)


newtype T2 a = T2 (a -> Int)
instance Functor T2 where
  fmap :: (a -> b) -> T2 a -> T2 b
  fmap f (T2 g) = error "T2 is not a Functor instance."

instance Contravariant T2 where
  contramap :: (a -> b) -> T2 b -> T2 a
  contramap f (T2 g) = T2 (g . f)


newtype T3 a = T3 (a -> a)
instance Functor T3 where
  fmap :: (a -> b) -> T3 a -> T3 b
  fmap f (T3 g) = error "T3 is not a Functor instance."
  -- f.g :: a -> b
  -- T3  :: (a -> a) -> T3

instance Invariant T3 where
  invmap :: (a -> b) -> (b -> a) -> T3 a -> T3 b
  invmap f g (T3 h) = T3 (f . h . g)

newtype T4 a = T4 ((Int -> a) -> Int)
instance Functor T4 where
  fmap :: (a -> b) -> T4 a -> T4 b
  fmap f (T4 g) = error "T4 is not a Functor instance."

instance Contravariant T4 where
  contramap :: (a -> b) -> T4 b -> T4 a
  contramap f (T4 g) = T4 (\h -> g (f . h))


newtype T5 a = T5 ((a -> Int) -> Int)
instance Functor T5 where
  fmap :: (a -> b) -> T5 a -> T5 b
  fmap f (T5 g) = T5 (\h -> g (h . f))
  -- T5 :: (b -> Int) -> Int
  -- f :: a -> b
  -- h :: b -> Int
  -- h . f :: a -> Int
  -- g :: (a -> Int) -> Int