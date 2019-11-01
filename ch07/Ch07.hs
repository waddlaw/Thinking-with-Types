{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
module Ch07 where

import Data.Foldable
import Data.Kind
import Data.Maybe
import Data.Typeable

data Any where
  Any :: a -> Any

anyList :: [Any]
anyList = [Any 5, Any True, Any "hello"]

elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any a) = f a

data HasShow where
  HasShow :: Show t => t -> HasShow

instance Show HasShow where
  -- show (HasShow s) = "HasShow " ++ show s
  show = elimHasShow show

elimHasShow :: (forall a. Show a => a -> r) -> HasShow -> r
elimHasShow f (HasShow a) = f a

data Dynamic where
  Dynamic :: Typeable t => t -> Dynamic

elimDynamic
  :: (forall a. Typeable a => a -> r)
  -> Dynamic
  -> r
elimDynamic f (Dynamic a) = f a

{-
λ> elimDynamic cast (Dynamic True) :: Maybe Int
Nothing
λ> elimDynamic cast (Dynamic True) :: Maybe Bool
Just True
-}

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

{-
λ> fromDynamic @Bool (Dynamic True)
Just True
λ> fromDynamic @Int (Dynamic True)
Nothing
-}

liftD2
  :: forall a b r. (Typeable a, Typeable b, Typeable r)
  => Dynamic
  -> Dynamic
  -> (a -> b -> r)
  -> Maybe Dynamic
liftD2 d1 d2 f = fmap Dynamic . f <$> fromDynamic @a d1 <*> fromDynamic @b d2

{-
λ> fromDynamic @Integer <$> liftD2 (Dynamic 1) (Dynamic 2) (+)
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
λ> fromDynamic @Int (pyPlus (Dynamic 1) (Dynamic 2))
Just 3
λ> fromDynamic @String (pyPlus (Dynamic "hello") (Dynamic " world"))
Just "hello world"
λ> fromDynamic @String (pyPlus (Dynamic 4) (Dynamic " minute"))
Just "4 minute"
-}

{-
asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty
-}

data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c

elimHas
  :: (forall a. c a => a -> r)
  -> Has c
  -> r
elimHas f (Has a) = f a
