{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
module Infinite where

type family F where
  F = F -> F

x :: F
x = x

-- Nope.
-- y :: F
-- y = undefined

-- Nope.
-- z :: F
-- z y = y

newtype Some where
  MkSome :: a -> Some

-- u :: Some
-- u = MkSome (x, x)
--   where
--     MkSome x = u

-- u :: Some
-- u = MkSome (x, x) where
--   x = case u of MkSome x -> x

u0 :: Some
u0 = case u of
  MkSome x -> MkSome (x, x)

u :: Some
u = MkSome (case u of MkSome x -> (x, x))