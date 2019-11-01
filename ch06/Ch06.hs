{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
module Ch06 where

applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5

f :: Int -> forall a. a -> a
f _ = id

g :: forall a b. (a -> b) -> ((forall c. c -> a) -> b)
g f = \h ->  f (h undefined)

h :: forall a b. (a -> b) -> (forall d .(forall c. c -> a) -> d)
h _ = \h -> undefined

f3 :: ((forall a. a -> a) -> Int) -> Bool -> Bool
f3 f b = (f id == 3) && b

f4 :: ((forall a. a -> a -> a) -> Int) -> Bool -> Bool
f4 f b = (f const == 3) && b

f5 :: ((forall a. a -> a -> a) -> Int) -> Bool -> Bool
f5 f b = f (if b then const else const id) == 42

h1 :: forall a b. (a -> b) -> (forall c. c -> a) -> b
h1 f g = f . g $ undefined

h2 :: forall a b c. (a -> b) -> (c -> a) -> b
h2 f g = f . g $ undefined

z :: (forall a. a) -> Int
z x = 0

z2 :: forall a. a -> Int
z2 x = 0

cont :: a -> (forall r. (a -> r) -> r)
cont a = \callback -> callback a

runCont :: (forall r. (a -> r) -> r) -> a
runCont f = f id
  -- let callback = id
  --  in f callback

newtype Cont a = Cont { unCont :: forall r. (a -> r) -> r }

-- Exercise 6.4-i
instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  -- fmap f (Cont g) = Cont (\h -> h (g f))
  -- fmap f (Cont g) = Cont (\h -> h . g $ f)
  -- fmap f (Cont g) = Cont (\h -> (.) h g $ f)
  -- fmap f (Cont g) = Cont (($f) . \h -> flip (.) g h)
  -- fmap f (Cont g) = Cont (($f) . flip (.) g)
  -- fmap f (Cont g) = Cont (($f) . flip (.) g)

  -- fmap f (Cont g) = Cont (\h -> h (g f))
  -- fmap f (Cont g) = Cont (\h -> h . g $ f)
  -- fmap f = (\g -> Cont (\h -> h . g $ f)) . unCont
  -- fmap f = (\g -> Cont (\h -> (.) h g $ f)) . unCont
  -- fmap f = (\g -> Cont (\h -> flip (.) g h $ f)) . unCont
  -- fmap f = (\g -> Cont (($f) . (\h -> flip (.) g h))) . unCont
  -- fmap f = (\g -> Cont (($f) . (flip (.) g))) . unCont
  -- fmap f = (\g -> Cont (($f) . (flip (.) g))) . unCont

  fmap f c = Cont $ cont $ f $ runCont $ unCont $ c

  -- f :: a -> b
  -- h :: (a -> r) -> r

  -- unCont :: Cont a -> (forall r. (a -> r) -> r)
  -- f :: (forall r1. (a -> r1) -> r1) -> (forall r2. (b -> r2) -> r2)
  -- Cont :: (forall r. (b -> r) -> r) -> Cont b

conv :: (a -> b) -> (forall r1. (a -> r1) -> r1) -> (forall r2. (b -> r2) -> r2)
conv f g = cont . f $ runCont g

-- (.) :: forall a c. ((forall r. (a -> r) -> r) -> c) -> (a -> (forall r. (a -> r) -> r)) -> (a -> c)

-- Cont . unCont == id

instance Applicative Cont where
  pure :: a -> Cont a
  pure a = Cont $ \callback -> callback a

  (<*>) :: Cont (a -> b) -> Cont a -> Cont b
  -- f :: forall r. ((a -> b) -> r) -> r
  -- c :: forall r. (   a     -> r) -> r
  -- fmap :: (a -> b) -> Cont a -> Cont b
  -- fmap f :: Cont ((a -> b) -> r) -> r
  -- (Cont f) <*> (Cont c) = Cont $ \callback -> callback (f c)
  -- (Cont f) <*> c = fmap (runCont f) c
  -- Cont f <*> Cont a = Cont $ \callback -> callback (f c)
  Cont f <*> a = fmap (f id) a

instance Monad Cont where
  (>>=) :: Cont a -> (a -> Cont b) -> Cont b
  Cont a >>= f = f (a id)

withVersionNumber :: (Double -> r) -> r
withVersionNumber f = f 1.0

withTimestamp :: (Int -> r) -> r
withTimestamp f = f 1532083362

withOS :: (String -> r) -> r
withOS f = f "linux"

releaseString :: String
releaseString =
  withVersionNumber $ \version ->
    withTimestamp $ \date ->
      withOS $ \os ->
        os ++ "-" ++ show version ++ "-" ++ show date

releaseStringCont :: String
releaseStringCont = runCont $ unCont $ do
  version <- Cont withVersionNumber
  date    <- Cont withTimestamp
  os      <- Cont withOS
  pure $ os ++ "-" ++ show version ++ "-" ++ show date