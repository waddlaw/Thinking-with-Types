# Fundamentals

```haskell
λ [minBound .. maxBound] :: [Unit]
[Unit]
λ [minBound .. maxBound] :: [Bool]
[False,True]
```

## The Algebra Behind Types

```haskell
λ cardinality @() undefined
1

λ cardinality @Bool undefined
2

λ cardinality @(Either Bool Bool) undefined
4
```

```shell
absurd :: Void -> a
absurd a = case a of {}
```

### Exercise

```haskell
λ cardinality @(Either Bool (Bool, Maybe Bool)) undefined
8
```

```haskell
from :: (b -> a, c -> a) -> (Either b c -> a)
from (f, g) = either f g

to :: (Either b c -> a) -> (b -> a, c -> a)
to f = (f . Left, f . Right)
```

```haskell
from :: (c -> (a, b)) -> (c -> a, c -> b)
from f = (fst . f, snd . f)

to :: (c -> a, c -> b) -> (c -> (a,b))
to (f, g) = \c -> (f c, g c)
```

### 論文

- [The Derivative of a Regular Type is its Type of One-Hole Contexts](http://strictlypositive.org/diff.pdf)
- [Higher-Order Type-Level Programming in Haskell](https://www.microsoft.com/en-us/research/uploads/prod/2019/03/unsaturated-type-families-icfp-2019.pdf)
- [Applying Type-Level and Generic Programming in Haskell](https://www.cs.ox.ac.uk/projects/utgp/school/andres.pdf)
- [Higher-order Type-level Programming in Haskell](https://www.imperial.ac.uk/media/imperial-college/faculty-of-engineering/computing/public/1718-ug-projects/Csongor-Kiss-Higher-order-type-level-programming-in-Haskell.pdf)
