{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Ch05_3 where

import Data.Kind (Constraint, Type)

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

{-
λ> hlist = Just "hello" :# True :# True :# HNil
λ> :t hlist
hlist :: HList '[Maybe [Char], Bool, Bool]
-}

hLength :: HList ts -> Int
hLength HNil    = 0
hLength (_:#ts) = 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

{-
λ> hHead HNil

<interactive>:5:7: error:
    • Couldn't match type ‘'[]’ with ‘t : ts0’
      Expected type: HList (t : ts0)
        Actual type: HList '[]
    • In the first argument of ‘hHead’, namely ‘HNil’
      In the expression: hHead HNil
      In an equation for ‘it’: it = hHead HNil
    • Relevant bindings include it :: t (bound at <interactive>:5:1)

λ> hHead (True :# 0 :# HNil)
True
-}

showBool :: HList '[_1, Bool, _2] -> String
showBool (_ :# b :# _ :# HNil) = show b

-- instance Eq (HList '[]) where
--   HNil == HNil = True

-- instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
--   (a :# as) == (b :# bs) = a == b && as == bs

{-
λ> HNil == HNil
True

λ> HNil == (True :# HNil)

<interactive>:6:10: error:
    • Couldn't match type ‘'[Bool]’ with ‘'[]’
      Expected type: HList '[]
        Actual type: HList '[Bool]
    • In the second argument of ‘(==)’, namely ‘(True :# HNil)’
      In the expression: HNil == (True :# HNil)
      In an equation for ‘it’: it = HNil == (True :# HNil)

λ> ('a' :# HNil)  == (True :# HNil)
<interactive>:8:20: error:
    • Couldn't match type ‘Bool’ with ‘Char’
      Expected type: HList '[Char]
        Actual type: HList '[Bool]
    • In the second argument of ‘(==)’, namely ‘(True :# HNil)’
      In the expression: ('a' :# HNil) == (True :# HNil)
      In an equation for ‘it’: it = ('a' :# HNil) == (True :# HNil)

λ> (False :# HNil)  == (True :# HNil)
False
λ> (True :# HNil)  == (True :# HNil)
True
-}

-- Exercise 5.3-i
-- instance Ord (HList '[]) where
--   (<=) :: HList '[] -> HList '[] -> Bool
--   HNil <= HNil = True
--   -- [] <= [] == True

-- instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
--   (<=) :: HList (t ': ts) -> HList (t ': ts) -> Bool
--   (a:#as) <= (b:#bs) = a <= b && as <= bs

{-
λ> (0 :# 1 :# HNil) <= (0 :# 10 :# HNil)
True

λ> (0 :# 1 :# HNil) <= (0 :# 1 :# HNil)
True

λ> (1 :# 1 :# HNil) <= (0 :# 1 :# HNil)
False
-}

-- Exercise 5.3-ii
-- instance Show (HList '[]) where
--   show :: HList '[] -> String
--   show _ = "[]"

-- instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
--   show :: HList (t ': ts) -> String
--   show (x :# xs) = show x <> " :# " <> show xs

{-
λ> show $ True :# HNil
"True :# []"
λ> show $ True :# (Just 100 :# Left 'a' :# [0,1,2] :# HNil) :# 3 :# HNil
"True :# Just 100 :# Left 'a' :# [0,1,2] :# [] :# 3 :# []"
-}

type family AllEq (ts :: [Type]) :: Constraint where
  AllEq '[] = ()
  AllEq (t ': ts) = (Eq t, AllEq ts)

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs

-- Exercise 5.3-iii
instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  (<=) :: HList ts -> HList ts -> Bool
  HNil <= HNil = True
  (a:#as) <= (b:#bs) = a <= b && as <= bs

instance (All Show ts) => Show (HList ts) where
  show :: HList ts -> String
  show xs = "[" <> showElems xs <> "]"

class ShowElems a where
  showElems :: a -> String

instance All Show ts => ShowElems (HList ts) where
  showElems :: HList ts -> String
  showElems HNil = ""
  showElems (x :# HNil) = show x
  showElems (x :# xs)   = show x <> ", " <> showElems xs

{-
λ> show $ True :# (Just 100 :# Left 'a' :# [0,1,2] :# HNil) :# 3 :# HNil
"[True, [Just 100, Left 'a', [0,1,2]], 3]"
-}