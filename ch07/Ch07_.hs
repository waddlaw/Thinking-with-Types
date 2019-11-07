{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Ch07_ where

import Data.Foldable
import Data.Kind
import Data.Maybe
import Data.Typeable

data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c

elimHas
  :: (forall a. c a => a -> r)
  -> Has c
  -> r
elimHas f (Has a) = f a

type HasShow = Has Show
type Dynamic = Has Typeable

instance Show HasShow where
  show = elimHas show

{-
λ> show (Has @Show True)
"True"
-}

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimHas cast

{-
λ> fromDynamic @Bool (Has True)
Just True
λ> fromDynamic @Int (Has True)
Nothing
-}

liftD2
  :: forall a b r. (Typeable a, Typeable b, Typeable r)
  => Dynamic
  -> Dynamic
  -> (a -> b -> r)
  -> Maybe Dynamic
liftD2 d1 d2 f = fmap Has . f <$> fromDynamic @a d1 <*> fromDynamic @b d2

{-
λ> fromDynamic @Integer <$> liftD2 (Has 1) (Has 2) (+)
Just (Just 3)
-}

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b =
  fromMaybe (error "bad types for pyPlus") $ asum
    [ liftD2 @String @String a b (++)
    , liftD2 @Int    @Int    a b (+)
    , liftD2 @String @Int    a b $ \strA intB -> strA ++ show intB
    , liftD2 @Int    @String a b $ \intA strB -> show intA ++ strB
    ]

{-
λ> fromDynamic @Int (pyPlus (Has 1) (Has 2))
Just 3
λ> fromDynamic @String (pyPlus (Has "hello") (Has " world"))
Just "hello world"
λ> fromDynamic @String (pyPlus (Has 4) (Has " minute"))
Just "4 minute"
-}

{-
asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty
-}

isMempty :: (Monoid a, Eq a) => a -> Bool
isMempty a = a == mempty

type MonoidAndEq a = (Monoid a, Eq a)

class    (Monoid a, Eq a) => MonoidEq a
instance (Monoid a, Eq a) => MonoidEq a

foo :: Has MonoidEq
foo = Has [True]