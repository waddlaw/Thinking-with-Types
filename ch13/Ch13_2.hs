{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
module Ch13_2 where

import GHC.Generics

-- a :: * -> *
class GEq a where
  geq :: a x -> a x -> Bool

-- Unit の U
instance GEq U1 where
  geq U1 U1 = True

-- Void の V
instance GEq V1 where
  geq _ _ = True

-- Kind の K
instance Eq a => GEq (K1 _1 a) where
  geq (K1 a) (K1 b) = a == b

-- Either
instance (GEq a, GEq b) => GEq (a :+: b) where
  geq (L1 a1) (L1 a2) = geq a1 a2
  geq (R1 b1) (R1 b2) = geq b1 b2
  geq _ _             = False

-- (,)
instance (GEq a, GEq b) => GEq (a :*: b) where
  geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 && geq b1 b2

-- D1, C1, S1
instance GEq a => GEq (M1 _x _y a) where
  geq (M1 a1) (M1 a2) = geq a1 a2

genericEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
genericEq a b = geq (from a) (from b)

{-
λ> :k! Rep Bool
Rep Bool :: * -> *
= D1
    ('MetaData "Bool" "GHC.Types" "ghc-prim" 'False)
    ( C1 ('MetaCons "False" 'PrefixI 'False) U1
      :+:
      C1 ('MetaCons "True"  'PrefixI 'False) U1
    )

λ> from True
M1 {unM1 = R1 (M1 {unM1 = U1})}

λ> from False
M1 {unM1 = L1 (M1 {unM1 = U1})}

λ> from Nothing
M1 {unM1 = L1 (M1 {unM1 = U1})}

λ> :t from False
from False
  :: D1
       ('MetaData "Bool" "GHC.Types" "ghc-prim" 'False)
       (C1 ('MetaCons "False" 'PrefixI 'False) U1
        :+: C1 ('MetaCons "True" 'PrefixI 'False) U1)
       x

λ> :t from Nothing
from Nothing
  :: D1
       ('MetaData "Maybe" "GHC.Maybe" "base" 'False)
       (C1 ('MetaCons "Nothing" 'PrefixI 'False) U1
        :+: C1
              ('MetaCons "Just" 'PrefixI 'False)
              (S1
                 ('MetaSel
                    'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                 (Rec0 a)))
       x
-}

data Foo a b c
  = F0
  | F1 a
  | F2 b c
  deriving stock    Generic
  deriving anyclass MyEq

instance (Eq a, Eq b, Eq c) => Eq (Foo a b c) where
  (==) = genericEq

class MyEq a where
  eq :: a -> a -> Bool
  default eq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
  eq a b = geq (from a) (from b)