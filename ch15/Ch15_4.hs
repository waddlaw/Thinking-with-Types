{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
module Ch15_4 where

import Data.Singletons.Prelude
import Data.Singletons.TH
import Unsafe.Coerce

singletons [d|
  data TimeOfDay
    = Morning
    | Afternoon
    | Evening
    | Night
    deriving (Eq, Ord, Show)
  |]

instance (Eq (Demote k), SingKind k) => SDecide k where
  a %~ b = if fromSing a == fromSing b
            then Proved $ unsafeCoerce Refl
            else Disproved $ const undefined

-- instance SDecide Bool where
--   STrue  %~ STrue  = Proved Refl
--   SFalse %~ SFalse = Proved Refl
--   _ %~ _ = Disproved $ const undefined

singletons [d|
  data MyMaybe a
    = MyNothing
    | MyJust a
    -- deriving (Eq, Ord, Show)
  |]

-- ex15.4-i
instance SDecide a => SDecide (MyMaybe a) where
  SMyNothing %~ SMyNothing = Proved Refl
  -- SMyJust a %~ SMyJust b = a %~ b
  SMyJust a %~ SMyJust b =
    case a %~ b of
      Proved Refl -> Proved Refl
      Disproved _ -> Disproved $ const undefined
  _ %~ _ = Disproved $ const undefined

{-
λ> :t MyJust
MyJust :: a -> MyMaybe a
λ> :t MyJust True
MyJust True :: MyMaybe Bool
λ> :k 'MyJust 'True
MyJust True :: MyMaybe Bool

λ> :t SMyJust
SMyJust :: Sing n -> Sing ('MyJust n)
λ> :t SMyJust STrue
SMyJust STrue :: Sing ('MyJust 'True)
-}