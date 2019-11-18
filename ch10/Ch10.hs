{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Ch10 where

import Prelude hiding (fst)

fst :: (a,b) -> a
fst (a, b) = a

data Fst a b = Fst (a, b)

class Eval l t | l -> t where
  eval :: l -> t

instance Eval (Fst a b) a where
  eval :: Fst a b -> a
  eval (Fst (a, b)) = a

-- Exercise 10.1-i
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (h:_) = Just h

data ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe a) (Maybe a) where
  eval :: ListToMaybe a -> Maybe a
  eval (ListToMaybe []) = Nothing
  eval (ListToMaybe (h:_)) = Just h

{-
λ> eval (ListToMaybe [])
Nothing
λ> eval (ListToMaybe [1,2])
Just 1
-}

data MapList dfb a = MapList (a -> dfb) [a]

instance Eval dfb dft => Eval (MapList dfb a) [dft] where
  eval :: MapList dfb a -> [dft]
  eval (MapList f []) = []
  eval (MapList f (a:as)) = eval (f a) : eval (MapList f as)

{-
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (a:as) = f a : map f as
-}