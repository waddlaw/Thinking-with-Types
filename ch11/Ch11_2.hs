{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Ch11_2 where

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import Fcf hiding (Any, type (-))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

data Any (f :: k -> Type) where
  Any :: f t -> Any f

data OpenProduct (f :: k -> Type) (ts :: [(Symbol, k)]) where
  OpenProduct :: V.Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

data Key (key :: Symbol) = Key

insert
  :: Eval (UniqueKey key ts) ~ 'True
  => Key key -> f t -> OpenProduct f ts -> OpenProduct f ('(key, t) ': ts)
insert _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

{-
λ> :t insert (Key @"another") (Just True) result
insert (Key @"another") (Just True) result
  :: OpenProduct Maybe '[ '("another", Bool), '("key", [Char])]
-}
result :: OpenProduct Maybe '[ '("key", [Char])]
result = insert (Key @"key") (Just "hello") nil

type UniqueKey (key :: k) (ts :: [(k, t)])
  = Null =<< Filter (TyEq key <=< Fst) ts

type FindElem (key :: Symbol) (ts :: [(Symbol, k)])
  = Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)

findElem :: forall key ts . KnownNat (FindElem key ts) => Int
findElem = fromIntegral . natVal $ Proxy @(FindElem key ts)

type LookupType (key :: k) (ts :: [(k ,t)])
  = FromMaybe Stuck =<< Lookup key ts

{-
λ> get (Key @"key") result
Just "hello"
-}
get
  :: forall key ts f. KnownNat (FindElem key ts)
  => Key key
  -> OpenProduct f ts
  -> f (Eval (LookupType key ts))
get _ (OpenProduct v) = unAny $ V.unsafeIndex v $ findElem @key @ts
  where
    unAny (Any a) = unsafeCoerce a

type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)])
  = SetIndex (FindElem key ts) '(key, t) ts

{-
λ> :t update (Key @"key") (Identity "a") result 
<interactive>:1:36: error:
    • Couldn't match type ‘Maybe’ with ‘Identity’
      Expected type: OpenProduct Identity '[ '("key", [Char])]
        Actual type: OpenProduct Maybe '[ '("key", [Char])]
    • In the third argument of ‘update’, namely ‘result’
      In the expression: update (Key @"key") (Identity "a") result

λ> :t update (Key @"key") (Just 0) result 
update (Key @"key") (Just 0) result
  :: Num t => OpenProduct Maybe '[ '("key", t)]

λ> get (Key @"key") $ update (Key @"key") (Just 0) result 
Just 0
-}
update :: forall key ts t f. KnownNat (FindElem key ts)
  => Key key -> f t -> OpenProduct f ts -> OpenProduct f (Eval (UpdateElem key t ts))
update _ ft (OpenProduct v) = OpenProduct $ v V.// [(findElem @key @ts, Any ft)]

-- Exercise 11.3-i
data DeleteIndex :: Nat -> [a] -> Exp [a]
type instance Eval (DeleteIndex n as) = DeleteIndexImpl n as
type family DeleteIndexImpl (n :: Nat) (as :: [k]) where
  DeleteIndexImpl _ '[] = '[]
  DeleteIndexImpl 0 (_ ': as) = as
  DeleteIndexImpl n (a ': as) = a ': DeleteIndexImpl (n-1) as

type DeleteElem (key :: Symbol) (ts :: [(Symbol, k)])
  = DeleteIndex (FindElem key ts) ts

{-
λ> delete (Key @"key") result
Just 0

λ> r2 = insert (Key @"another") (Just True) result
λ> :t r2
r2 :: OpenProduct Maybe '[ '("another", Bool), '("key", [Char])]

λ> :t delete (Key @"key") r2
delete (Key @"key") r2 :: OpenProduct Maybe '[ '("another", Bool)]
λ> :t delete (Key @"another") r2
delete (Key @"another") r2
  :: OpenProduct Maybe '[ '("key", [Char])]
-}
delete :: forall key ts t f. KnownNat (FindElem key ts)
  => Key key -> OpenProduct f ts -> OpenProduct f (Eval (DeleteElem key ts))
delete _ (OpenProduct v) =  OpenProduct v1
  where
    (v1, v2) = V.splitAt (findElem @key @ts) v

-- Exercise 11.3-ii
-- TODO

instance (key ~ key') => IsLabel key (Key key') where
  fromLabel = Key

{-
λ> :t get #key result
get #key result :: Maybe [Char]
λ> get #key result
Just "hello"
-}