{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
module Ch10_2 where

import Data.Kind (Constraint, Type)
import GHC.TypeLits

type Exp a = a -> Type
type family Eval (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b
type instance Eval (Snd '(a, b)) = b

data FromMaybe :: a -> Maybe a -> Exp a
type instance Eval (FromMaybe _1 ('Just a)) = a
type instance Eval (FromMaybe a 'Nothing)   = a

-- Exercise 10.2-i
data ListToMaybe :: [a] -> Exp (Maybe a)
type instance Eval (ListToMaybe '[]) = 'Nothing
type instance Eval (ListToMaybe (x ': _)) = 'Just x

{-
λ> :k! Eval (ListToMaybe '[1,2,3])
Eval (ListToMaybe '[1,2,3]) :: Maybe GHC.Types.Nat
= 'Just 1
-}

data MapList :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval (MapList f '[]) = '[]
type instance Eval (MapList f (a ': as)) = Eval (f a) ': Eval (MapList f as)

{-
λ> :k! Eval (MapList (FromMaybe 0) '[Nothing, ('Just 1)])
Eval (MapList (FromMaybe 0) '[Nothing, ('Just 1)]) :: [GHC.Types.Nat]
= '[0, 1]

λ> :k! Eval (MapList Snd ['(5, 7), '(13, 13)])
Eval (MapList Snd ['(5, 7), '(13, 13)]) :: [GHC.Types.Nat]
= '[7, 13]
-}

-- Exercise 10.2-ii
data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance Eval (Foldr op acc '[]) = acc
type instance Eval (Foldr op acc (x ': xs)) = Eval (x `op` (Eval (Foldr op acc xs)))

data Add :: Nat -> Nat -> Exp Nat
type instance Eval (Add x y) = x + y

{-
λ> :k! Eval (Foldr Add 0 '[1,2,3,4,5])
Eval (Foldr Add 0 '[1,2,3,4,5]) :: Nat
= 15
-}

data Pure :: a -> Exp a
type instance Eval (Pure x) = x

data (=<<) :: (a -> Exp b) -> Exp a -> Exp b
type instance Eval (k =<< e) = Eval (k (Eval e))
infixr 0 =<<

data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> (a -> Exp c)
type instance Eval ((f <=< g) x) = Eval (f (Eval (g x)))
infixr 1 <=<

{-
λ> :k! Eval (Snd2 '(1, '(2, 3)))
Eval (Snd2 '(1, '(2, 3))) :: Nat
= 3

λ> :k! Eval (Snd <=< FromMaybe '(0,0) =<< Pure (Just '(1,2)))
Eval (Snd <=< FromMaybe '(0,0) =<< Pure (Just '(1,2))) :: Nat
= 2
-}

data TyEq :: a -> b -> Exp Bool
type instance Eval (TyEq a b) = TyEqImpl a b
type family TyEqImpl (a :: k) (b :: k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl a b = 'False

data Collapse :: [Constraint] -> Exp Constraint
type instance Eval (Collapse '[]) = (() :: Constraint)
type instance Eval (Collapse (a ': as)) = (a, Eval (Collapse as))

type All (c :: k -> Constraint) (ts :: [k]) = Collapse =<< MapList (Pure1 c) ts

data Pure1 :: (a -> b) -> a -> Exp b
type instance Eval (Pure1 f x) = f x

{-
λ> :k! Eval (All Eq '[Int, Bool])
Eval (All Eq '[Int, Bool]) :: Constraint
= (Eq Int, (Eq Bool, () :: Constraint))
-}

data Map :: (a -> Exp b) -> f a -> Exp (f b)
type instance Eval (Map f '[]) = '[]
type instance Eval (Map f (a ': as)) = Eval (f a) ': Eval (Map f as)

type instance Eval (Map f 'Nothing)  = 'Nothing
type instance Eval (Map f ('Just a)) = 'Just (Eval (f a))

type instance Eval (Map f ('Left x))  = 'Left x
type instance Eval (Map f ('Right a)) = 'Right (Eval (f a))

{-
λ> :k! Eval (Map Snd ('Just '(1,2)))
Eval (Map Snd ('Just '(1,2))) :: Maybe Nat
= 'Just 2

λ> :k! Eval (Map Snd '[ '(1,2)])
Eval (Map Snd '[ '(1,2)]) :: [Nat]
= '[2]

λ> :k! Eval (Map Snd (Left 'False))
Eval (Map Snd (Left 'False)) :: Either Bool b
= 'Left 'False
-}

-- Exercise 10.4-i
type instance Eval (Map f '(a, b)) = '(a, Eval (f b))

{-
λ> :k! Eval (Map Snd '(0, '(1,2)))
Eval (Map Snd '(0, '(1,2))) :: (Nat, Nat)
= '(0, 2)
-}

data Fmap :: (a -> b) -> Exp a -> Exp b
type instance Eval (Fmap f p) = Eval ((Pure1 f) =<< p)

{-
λ> :k! Eval (Fmap Maybe (Snd '(Int,Bool)))
Eval (Fmap Maybe (Snd '(Int,Bool))) :: Type
= Maybe Bool
-}

-- type family (++) (a :: [k]) (b :: [k]) :: [k] where
--   '[] ++ ys       = ys
--   (x ': xs) ++ ys = x ': (xs ++ ys)
-- infixr 6 ++

data Mappend :: a -> a -> Exp a
type instance Eval (Mappend '() '()) = '()
type instance Eval (Mappend (a :: Constraint) (b :: Constraint)) = (a, b)
type instance Eval (Mappend (a :: [k]) (b :: [k])) = Eval (a ++ b)

data (++) :: [k] -> [k] -> Exp [k]
type instance Eval ('[] ++ ys) = ys
type instance Eval ((x ': xs) ++ ys) = x ': Eval (xs ++ ys)

data Mempty :: k -> Exp k

type instance Eval (Mempty '()) = '()
type instance Eval (Mempty (c :: Constraint)) = (() :: Constraint)
type instance Eval (Mempty (l :: [k])) = '[]

{-
λ> :k! Eval (Mempty '())
Eval (Mempty '()) :: ()
= '()

λ> :k! Eval (Mempty '[1])
Eval (Mempty '[1]) :: [Nat]
= '[]
-}