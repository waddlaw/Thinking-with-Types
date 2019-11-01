# 6 Rank-N Types

## 6.3 The Nitty Gritty Details

Exercise 6.3-i

```haskell
f :: Int -> forall a. a -> a
f :: Int -> (forall a. (a -> a))
f :: Int -> RANK1
f :: RANK1

--
f :: Int -> forall a. a -> a
f :: forall a. Int -> a -> a
f :: RANK1
```

Exercise 6.3-ii

```haskell
g :: (a -> b) -> (forall c. c -> a) -> b
g :: (a -> b) -> ((forall c. c -> a) -> b)
g :: (a -> b) -> (RANK1 -> b)
g :: (a -> b) -> RANK2
g :: RANK2

--
g :: (a -> b) -> (forall c. c -> a) -> b
g :: forall a b. (a -> b) -> ((forall c. c -> a) -> b)
g :: forall a b. (a -> b) -> (RANK1 -> b)
g :: forall a b. (a -> b) -> RANK1 -> b
g :: RANK2
```

Exercise 6.3-iii

```haskell
h :: ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a
h :: (RANK1 -> b (z m a)) -> m a
h :: RANK2 -> m a
h :: RANK3

--
h :: ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a
h :: forall m a b z. ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a
h :: forall m a b z. (RANK1 -> b (z m a)) -> m a
h :: forall m a. RANK2 -> m a
h :: RANK3

--
h :: ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a
h :: forall m a b z. ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a
h :: forall m a b z. ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a
```

### memo1

なぜか `Rank2Types` で `Rank3` 多相が定義できる。

> The obsolete language options PolymorphicComponents and Rank2Types are synonyms for RankNTypes.

- [9.21. Arbitrary-rank polymorphism](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#arbitrary-rank-polymorphism)

### memo2

> The RankNTypes option is also required for any type with a forall or context to the right of an arrow (e.g. f :: Int -> forall a. a->a, or g :: Int -> Ord a => a -> a). Such types are technically rank 1, but are clearly not Haskell-98, and an extra extension did not seem worth the bother.

- [Rank-N types](https://wiki.haskell.org/Rank-N_types)

### memo3

`forall a. a -> (forall b. b -> a)` と `forall a b. a -> b -> a` は同じ

```haskell
const :: a -> (forall b. b -> a)
const :: a -> RANK1
const :: RANK1

const :: forall a. a -> (forall b. b -> a)
const :: forall a. a -> RANK1
const :: RANK1
```

`forall b. (forall a. a -> a) -> b` と `forall a b. (a -> a) -> b` は違う

## 6.4

### Exercise 6.4-i

```haskell
instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  fmap f (Cont g) = Cont (($f) . flip (.) g)
```

```haskell
instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  fmap f = (\g -> Cont (($f) . (flip (.) g))) . unCont
```

```haskell
instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  fmap f c = Cont $ cont $ f $ runCont $ unCont c
```

模範解答

```haskell
instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  fmap f (Cont c) = Cont $ \c' -> c (c' . f)
```

functor law: `fmap id (Cont c)= id (Cont c)`

```haskell
fmap id (Cont c)
  = Cont $ \c' -> c (c' . id)
  = Cont $ \c' -> c c'
  = Cont c
```

### Exercise 6.4-ii

```haskell
instance Applicative Cont where
  pure :: a -> Cont a
  pure a = Cont $ \callback -> callback a

  (<*>) :: Cont (a -> b) -> Cont a -> Cont b
  Cont f <*> a = fmap (f id) a

  -- Cont f <*> Cont a = Cont $ \callback -> callback (f c)
```

Applicative law (identity): `pure id <*> Cont c = Cont c`

```hs
pure id <*> Cont c
  = (Cont $ \callback -> callback id) <*> Cont c
  = fmap (\callback -> callback id $ id) (Cont c)
  = fmap (id id) (Cont c)
  = fmap id (Cont c)
  = Cont c
```

Applicative law (composition): `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`

```hs
-- fmap f (Cont c) = Cont $ \c' -> c (c' . f)
pure (.) <*> (Cont u) <*> (Cont v) <*> (Cont w)
  = ((Cont (\callback -> callback (.)) <*> (Cont u)) <*> (Cont v)) <*> (Cont w)
  = ((fmap ((\callback -> callback (.)) id) (Cont u)) <*> (Cont v)) <*> (Cont w)
  = ((fmap (id (.)) (Cont u)) <*> (Cont v)) <*> (Cont w)
  = ((fmap (.) (Cont u)) <*> (Cont v)) <*> (Cont w)
  = ((Cont $ \c' -> u (c' . (.))) <*> (Cont v)) <*> (Cont w)
  = ...
```

### Exercise 6.4-iii

```haskell
instance Monad Cont where
  (>>=) :: Cont a -> (a -> Cont b) -> Cont b
  Cont a >>= f = f (a id)
```

Monad law (1): `return a >>= k  =  k a`
Monad law (2): `m >>= return  =  m`
Monad law (3): `m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h`
