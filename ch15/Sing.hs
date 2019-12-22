{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Sing where

import Data.Kind (Type)
import Data.Typeable
import Data.Void
import Unsafe.Coerce (unsafeCoerce)

data family Sing (a :: k)

data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

withSomeSing :: SomeSing k -> (forall (a :: k). Sing a -> r) -> r
withSomeSing (SomeSing s) f = f s

class SingKind k where
  type Demote k = r | r -> k
  toSing :: Demote k -> SomeSing k
  fromSing :: Sing (a :: k) -> Demote k

data instance Sing (a :: Bool) where
  STrue  :: Sing 'True
  SFalse :: Sing 'False

instance SingKind Bool where
  type Demote Bool = Bool
  toSing True  = SomeSing STrue
  toSing False = SomeSing SFalse
  fromSing STrue  = True
  fromSing SFalse = False

class SingI (a :: k) where
  sing :: Sing a

instance SingI 'True where
  sing = STrue

instance SingI 'False where
  sing = SFalse

{-
λ> :t sing @'True

<interactive>:1:7: error:
    • Expected a type, but ‘ 'True’ has kind ‘Bool’
    • In the type ‘ 'True’
      In the expression: sing @ 'True

λ> fromSing (sing :: Sing True)
True

λ> :t sing
sing :: forall k (a :: k). SingI a => Sing a

λ> :t sing @Bool @True
sing @Bool @True :: Sing 'True
-}

x1, x2 :: Bool
x1 = fromSing (sing :: Sing True)
x2 = fromSing (sing @Bool @True)

data instance Sing (a :: Maybe k) where
  SJust    :: Sing (a :: k) -> Sing ('Just a)
  SNothing :: Sing 'Nothing

instance SingI a => SingI ('Just a) where
  sing = SJust sing

instance SingI 'Nothing where
  sing = SNothing

instance (k ~ Demote k, SingKind k) => SingKind (Maybe k) where
  type Demote (Maybe k) = Maybe k
  toSing (Just a) = withSomeSing (toSing a) $ SomeSing . SJust
  toSing Nothing = SomeSing SNothing
  fromSing (SJust a) = Just $ fromSing a
  fromSing SNothing = Nothing

data instance Sing (a :: [k]) where
  SNil  :: Sing '[]
  SCons :: Sing (h :: k) -> Sing (t :: [k]) -> Sing (h ': t)

instance (k ~ Demote k, SingKind k) => SingKind [k] where
  type Demote [k] = [k]
  toSing [] = SomeSing SNil
  toSing (h : t) =
    withSomeSing (toSing h) $ \sh ->
      withSomeSing (toSing t) $ \st ->
        SomeSing $ SCons sh st
  fromSing SNil = []
  fromSing (SCons sh st) = fromSing sh : fromSing st

-- Exercise 15.3-i
instance SingI '[] where
  sing = SNil

instance (SingI a, SingI as) => SingI (a ': as) where
  sing = SCons sing sing

{-
λ> fromSing $ sing @[Bool] @[True,False,True]
[True,False,True]

λ> fromSing $ sing @[Bool] @'[]
[]
-}