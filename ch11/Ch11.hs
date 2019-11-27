{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Ch11 where

import Data.Kind (Type, Constraint)
import Data.Proxy
import Fcf
import GHC.TypeLits hiding (type (+))
import Unsafe.Coerce
import Data.Functor.Identity

data OpenSum (f :: k -> Type) (ts :: [k]) where
  UnsafeOpenSum :: Int -> f t -> OpenSum f ts

-- Stuck は型レベル undefined
type FindElem (key  :: k) (ts :: [k]) = FromMaybe Stuck =<< FindIndex (TyEq key) ts

{-
λ> :k! Member Bool '[]
Member Bool '[] :: Constraint
= KnownNat Stuck

λ> :k! Member Bool '[Bool]
Member Bool '[Bool] :: Constraint
= KnownNat 0

λ> :k! Member Bool '[Int]
Member Bool '[Int] :: Constraint
= KnownNat Stuck

λ> :k! Member Bool '[Int,Bool]
Member Bool '[Int,Bool] :: Constraint
= KnownNat 1
-}
type Member t ts = KnownNat (Eval (FindElem t ts))

{-
λ> findElem @Int @'[Int]
0
λ> findElem @Int @'[Bool, Int, Int]
1
λ> findElem @Int @'[Bool, String,Int]
2
-}
findElem :: forall t ts. Member t ts => Int
findElem = fromIntegral . natVal $ Proxy @(Eval (FindElem t ts))

{-
-- 実際には Show インスタンスはまだない
λ> inj @Maybe @Int @'[Bool, Int] Nothing
UnsafeOpenSum 1 Nothing

λ> inj @Maybe @Int @'[Bool, Int] (Just 1)
UnsafeOpenSum 1 (Just 1)

λ> inj @Identity @Bool @'[Bool, Int] (Identity True)
UnsafeOpenSum 0 (Identity True)
-}
inj :: forall f t ts. Member t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)

{-
λ> prj @Identity @Bool @'[Bool, Int]  $ inj @Identity @Bool @'[Bool, Int] (Identity True)
Just (Identity True)

λ> prj @Maybe @Int @'[Bool, Int]  $ inj @Maybe @Int @'[Bool, Int] (Just 1)
Just (Just 1)

λ> prj @Maybe @Bool @'[Bool, Int]  $ inj @Maybe @Int @'[Bool, Int] (Just 1)
Nothing
-}
prj :: forall f t ts. Member t ts => OpenSum f ts -> Maybe (f t)
prj (UnsafeOpenSum i f) =
  if i == findElem @t @ts
    then Just $ unsafeCoerce f
    else Nothing

{-
λ> s = inj @Identity @Bool @'[Bool, Int] (Identity True)
λ> :t decompose s
decompose s :: Either (Identity Bool) (OpenSum Identity '[Int])
-}
decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 t) = Left  $ unsafeCoerce t
decompose (UnsafeOpenSum n t) = Right $ UnsafeOpenSum (n-1) t

-- Exercise 11.2-i
{-
λ> s = inj @Identity @Bool @'[Bool, Int] (Identity True)
λ> :t weaken s
weaken s :: OpenSum Identity '[x, Bool, Int]
-}
weaken :: OpenSum f ts -> OpenSum f (x ': ts)
weaken (UnsafeOpenSum n t) = UnsafeOpenSum (n+1) t

{-
λ> s = inj @Identity @Bool @'[Bool, Int] (Identity True)
λ> match (const 10) s
10
-}
match :: forall f ts b. (forall t. f t -> b) -> OpenSum f ts -> b
match fn (UnsafeOpenSum _ t) = fn t
