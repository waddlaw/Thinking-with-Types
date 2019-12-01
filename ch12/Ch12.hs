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
module Ch12 where

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
-- type Member t ts = KnownNat (Eval (FindElem t ts))
type Member f t ts = KnownNat (Eval (FriendlyFindElem f t ts))

{-
λ> findElem @Int @'[Int]
0
λ> findElem @Int @'[Bool, Int, Int]
1
λ> findElem @Int @'[Bool, String,Int]
2
-}
findElem :: forall f t ts. Member f t ts => Int
findElem = fromIntegral . natVal $ Proxy @(Eval (FriendlyFindElem f t ts))

{-
-- 実際には Show インスタンスはまだない.定義しようと思ったけど普通には無理だった。(実力不足なだけかも)
λ> inj @Maybe @Int @'[Bool, Int] Nothing
UnsafeOpenSum 1 Nothing

λ> inj @Maybe @Int @'[Bool, Int] (Just 1)
UnsafeOpenSum 1 (Just 1)

λ> inj @Identity @Bool @'[Bool, Int] (Identity True)
UnsafeOpenSum 0 (Identity True)
-}
inj :: forall f t ts. Member f t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElem @f @t @ts)

{-
λ> prj @Identity @Bool @'[Bool, Int]  $ inj @Identity @Bool @'[Bool, Int] (Identity True)
Just (Identity True)

λ> prj @Maybe @Int @'[Bool, Int]  $ inj @Maybe @Int @'[Bool, Int] (Just 1)
Just (Just 1)

λ> prj @Maybe @Bool @'[Bool, Int]  $ inj @Maybe @Int @'[Bool, Int] (Just 1)
Nothing
-}
prj :: forall f t ts. Member f t ts => OpenSum f ts -> Maybe (f t)
prj (UnsafeOpenSum i f) =
  if i == findElem @f @t @ts
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

-- Ch12
{-
λ> let foo = inj (Identity True) :: OpenSum Identity '[Bool, String]
λ> prj foo :: Maybe (Identity Int)

<interactive>:6:1: error:
    • No instance for (KnownNat Stuck) arising from a use of ‘prj’
    • In the expression: prj foo :: Maybe (Identity Int)
      In an equation for ‘it’: it = prj foo :: Maybe (Identity Int)

λ> prj foo :: Maybe (Identity Bool)
Just (Identity True)
-}

{-
λ> 1 True

<interactive>:8:1: error:
    • Non type-variable argument in the constraint: Num (Bool -> t)
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall t. Num (Bool -> t) => t
-}

type family FriendlyFindElem (f :: k -> Type) (t :: k) (ts :: [k]) where
  FriendlyFindElem f t ts =
    FromMaybe
      ( TypeError
      ( 'Text "Attempted to call `friendlyPrj' to produce a `"
    ':<>: 'ShowType (f t)
    ':<>: 'Text "'."
    ':$$: 'Text "But the OpenSum can only contain one of:"
    ':$$: 'Text "  "
    ':<>: 'ShowType ts
    )) =<< FindIndex (TyEq t) ts

type family PrettyList (ts :: [k]) where
  PrettyList '[] = 'Text ""
  PrettyList (t ': ts) = 'ShowType t ':<>: 'Text ", " ':<>: PrettyList ts

{-
λ> let foo = inj (Identity True) :: OpenSum Identity '[Bool, String]
λ> prj foo :: Maybe (Identity Int)

<interactive>:5:1: error:
    • Attempted to call `friendlyPrj' to produce a `Identity Int'.
      But the OpenSum can only contain one of:
        '[Bool, String]
    • In the expression: prj foo :: Maybe (Identity Int)
      In an equation for ‘it’: it = prj foo :: Maybe (Identity Int)
-}