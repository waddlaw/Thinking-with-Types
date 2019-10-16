# 2.1 The Kind System

```haskell
λ> :k Int
Int :: *

λ> :k Maybe Bool
Maybe Bool :: *

λ> :set -XNoStarIsType
λ> :k Int
Int :: Type

λ> :k Maybe Bool
Maybe Bool :: Type
```

カインドが `Type` ではない型

```haskell
λ> :k Maybe
Maybe :: Type -> Type
```

その他の型のカインド

```haskell
λ> :k (->)
(->) :: Type -> Type -> Type

λ> :k Either
Either :: Type -> Type -> Type
```

`MaybeT` モナド変換子

```haskell
λ> import Control.Monad.Trans.Maybe

λ> :t MaybeT
MaybeT :: m (Maybe a) -> MaybeT m a

λ> :k MaybeT
MaybeT :: (Type -> Type) -> Type -> Type
```

## 用語

- HKTs (Higher-kinded types)
  - 型変数を持つやつ

## カインドの種類

- `Type`
- `Constraint`

## 演習

Exercise 2.1.3-i

```haskell
λ> :i Show
class Show a where
...

λ> :k Show
Show :: Type -> Constraint
```

Exercise 2.1.3-ii

```haskell
λ> :i Functor
class Functor (f :: Type -> Type) where
...

λ> :k Functor
Functor :: (Type -> Type) -> Constraint
```

Exercise 2.1.3-iii

```haskell
λ> :i Monad
class Applicative m => Monad (m :: Type -> Type) where
...

λ> :k Monad
Monad :: (Type -> Type) -> Constraint
```

Exercise 2.1.3-iv

```haskell
λ> import Control.Monad.Trans.Class

λ> :i MonadTrans
class MonadTrans (t :: (Type -> Type) -> Type -> Type) where

λ> :k MonadTrans
MonadTrans :: ((Type -> Type) -> Type -> Type) -> Constraint
```

# 2.2 Data Kinds

- `-XDataKinds`

```haskell
λ> :k True
True :: Bool
λ> :k 'True
'True :: Bool

λ> :k False
False :: Bool
λ> :k 'False
'False :: Bool
```

クォートの有無について

```haskell
λ> data SimpleData a = SimpleData a

λ> :k SimpleData
SimpleData :: Type -> Type

λ> :k 'SimpleData
'SimpleData :: a -> SimpleData a
```

```haskell
λ> data Unit = Unit
λ> :k Unit
Unit :: Type
λ> :k 'Unit
'Unit :: Unit

λ> :k Maybe Unit
Maybe Unit :: Type
λ> :k Maybe 'Unit

<interactive>:1:7: error:
    • Expected a type, but ‘ 'Unit’ has kind ‘Unit’
    • In the first argument of ‘Maybe’, namely ‘ 'Unit’
      In the type ‘Maybe  'Unit’
```

型レベル文字列

```haskell
λ> import GHC.TypeLits
λ> :k "hello"
"hello" :: Symbol

λ> :kind! AppendSymbol  "thinking" " with types"
AppendSymbol  "thinking" " with types" :: Symbol
= "thinking with types"

λ> :k AppendSymbol
AppendSymbol :: Symbol -> Symbol -> Symbol
```

`CmpSymbol`

```haskell
λ> :k CmpSymbol
CmpSymbol :: Symbol -> Symbol -> Ordering

λ> :kind! CmpSymbol "sandy" "sandy"
CmpSymbol "sandy" "sandy" :: Ordering
= 'EQ

λ> :kind! CmpSymbol "sandy" "batman"
CmpSymbol "sandy" "batman" :: Ordering
= 'GT
```

型レベルリスト

```haskell
λ> :k [Bool]
[Bool] :: Type

λ> :k '[Bool]
'[Bool] :: [Type]
```

proposal

- [-XTopLevelKindSignatures #235](https://github.com/ghc-proposals/ghc-proposals/pull/235)
  - [TopLevelKindSignatures -> StandaloneKindSignatures #258](https://github.com/ghc-proposals/ghc-proposals/pull/258)
  - [Standalone kind signatures (#16794)](https://gitlab.haskell.org/ghc/ghc/merge_requests/1438)
- [Unsaturated type families #242](https://github.com/ghc-proposals/ghc-proposals/pull/242)
  - [Higher-order type-level programming in Haskell](https://www.microsoft.com/en-us/research/publication/higher-order-type-level-programming-in-haskell/)