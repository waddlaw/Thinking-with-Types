{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Memo2 where

import Data.Kind

data family Sing (a::k)

data SomeSing :: Type -> Type where
  SomeSing :: forall (a::k). Sing a -> SomeSing k

withSomeSing :: SomeSing k -> (forall (a::k). Sing a -> r) -> r
withSomeSing (SomeSing s) f = f s

class SingKind k where
  type Demote k = r | r -> k
  toSing   :: Demote k    -> SomeSing k
  fromSing :: Sing (a::k) -> Demote k

-- data instance Sing (a :: Bool) where
--   STrue  :: Sing 'True
--   SFalse :: Sing 'False