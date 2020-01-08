{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
module Example where

import Data.Proxy
import Data.Kind

data SMaybe :: Maybe Bool -> Type where
  SNothing :: SMaybe 'Nothing
  SJust    :: forall (a::Bool). Proxy Bool -> SMaybe ('Just a)

  -- SJust    :: forall ('True::Bool). Proxy Bool -> SMaybe ('Just 'True)

-- SJust True
-- SJust STrue :: SMaybe ('Just 'True) :: Type

-- fromSMaybe :: forall (a::Maybe k). SMaybe a -> Maybe (Proxy k)
-- fromSMaybe SNothing = Nothing
-- fromSMaybe (SJust a) = Just a

{-
λ> :t SJust SNothing 
SJust SNothing :: SMaybe ('Just (SMaybe 'Nothing))

λ> :t SJust True
SJust True :: SMaybe ('Just Bool)
-}