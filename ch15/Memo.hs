{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Memo where

import Data.Kind

-- data MyMaybe a
--   = MyJust a
--   | MyNothing

-- data MyMaybe a where
--   MyJust    :: a -> MyMaybe a
--   MyNothing :: MyMaybe a

-- data MyMaybe :: Type -> Type where
--   MyJust    :: a -> MyMaybe a
--   MyNothing :: MyMaybe a

data MyMaybe :: Type -> Type where
  MyJust    :: forall (a::Type). a -> MyMaybe a
  MyNothing :: forall (a::Type). MyMaybe a

data SBool (a :: Bool) where
  STrue  :: SBool 'True
  SFalse :: SBool 'False

fromSBool :: forall (a::Bool). SBool a -> Bool
fromSBool STrue  = True
fromSBool SFalse = False

data SomeSBool where
  SomeSBool :: forall (a::Bool). SBool a -> SomeSBool

toSBool :: Bool -> SomeSBool
toSBool True  = SomeSBool STrue
toSBool False = SomeSBool SFalse

withSomeSBool :: SomeSBool -> (forall (a :: Bool). SBool a -> r) -> r
withSomeSBool (SomeSBool s) f = f s

matchSBool :: (forall (a :: Bool). SBool a -> r) -> Bool -> r
matchSBool f b = withSomeSBool (toSBool b) f

showSBool :: forall (a::Bool). SBool a -> String
showSBool STrue  = "True"
showSBool SFalse = "False"

data SMaybe :: Maybe Bool -> Type where
  SNothing :: SMaybe 'Nothing
  SJust    :: forall (a::Bool). SBool a -> SMaybe ('Just a)

fromSMaybe :: forall (a::Maybe Bool). SMaybe a -> Maybe Bool
fromSMaybe SNothing  = Nothing
fromSMaybe (SJust a) = Just (fromSBool a)

data SomeSMaybe where
  SomeSMaybe :: forall (a::Maybe Bool). SMaybe a -> SomeSMaybe

toSMaybe :: Maybe Bool -> SomeSMaybe
toSMaybe Nothing = SomeSMaybe SNothing
toSMaybe (Just a) = withSomeSBool (toSBool a) (SomeSMaybe . SJust)

withSomeSMaybe :: SomeSMaybe -> (forall (a :: Maybe Bool). SMaybe a -> r) -> r
withSomeSMaybe (SomeSMaybe s) f = f s