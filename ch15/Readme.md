# Ch15. Dependent Types

## DataKinds

`DataKinds` によって型がカインドに昇格 (Promote) する

値 | 型 | Kind
----|----|------
True, False | Bool | Type

`DataKinds` を有効にした後

値 | 型 | Kind
----|----|------
True, False | Bool | Type
- | 'True, (True) | Bool
- | 'False, (False) | Bool

- 型とカインドは同じ名前空間を持つ
- `'` は名前が被らなければ付けなくても良いが、個人的には常時付けた方が良いと思う

```hs
λ> :set -XDataKinds

λ> data MyBool = MyTrue | MyFalse

λ> :k MyTrue
MyTrue :: MyBool

λ> :k 'MyTrue
'MyTrue :: MyBool
```

## Type kind

値 | 型 | Kind
----|----|------
- | (->) | Type -> Type -> Type

- Haskell の値は `Type` カインドを持つ型のみ！
- `(->) :: Type -> Type -> Type`
  - `(->)` には `Type` カインドを持つ型の値しか適用できない！

## singleton とは何か

`singleton` というのは `()` 型のように型に対して値が1つしか存在しない型のこと。

値が1つしかないため、型がわかれば自動的に値が決まる。

```hs
f :: ()
f = ()

g :: Bool
g = ??? -- この場合 True, False の両方の可能性があるため値が何かというのは型を見るだけでは決定できない
```

すぐに思いつくのは以下のような書き方だけども、これは2つの理由でうまく行かない。

```hs
t :: 'True
t = True
```

1. Haskell の値は `Type` カインドを持たなければならないが、`Bool` カインドである
2. `True` データコンストラクタは `Bool` 型の値であり、`'True` 型の値ではない。

この問題を解決するために `SBool` と `fromSBool` が必要となる。

## GADTs

```hs
data MyMaybe a
  = MyJust a
  | MyNothing

-- GADTs で書き直すと
data MyMaybe a where
  MyJust    :: a -> MyMaybe a
  MyNothing :: MyMaybe a
```

ここで型変数のスコープに注意。

`Maybe a` の `a` はスコープを持ちません。そのため以下のように書いても良い

```hs
data MyMaybe :: Type -> Type where
  MyJust    :: a -> MyMaybe a
  MyNothing :: MyMaybe a
```

さらに `forall` を明示的に追加するとこうなる。

```hs
data MyMaybe :: Type -> Type where
  MyJust    :: forall (a::Type). a -> MyMaybe a
  MyNothing :: forall (a::Type). MyMaybe a
```

- [Are type variables in GADT heads meaningful?](https://stackoverflow.com/questions/40787526/are-type-variables-in-gadt-heads-meaningful)
- [9.4.7. Declaring data types with explicit constructor signatures](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#declaring-data-types-with-explicit-constructor-signatures)

## 15.2 Bool の例

<iframe style="border: none;" width="800" height="450" src="https://www.figma.com/embed?embed_host=share&url=https%3A%2F%2Fwww.figma.com%2Ffile%2FAgquwTzP53l6rMGqvqNRjV%2FCh15%3Fnode-id%3D34%253A0" allowfullscreen></iframe>

`SBool` の `S` は `Singleton` の意味。

```hs
data SBool (a :: Bool) where
  STrue  :: SBool 'True
  SFalse :: SBool 'False
```

`SBool` の定義の後

値 | 型 | Kind
----|----|------
True, False | Bool | Type
- | 'True | Bool
- | 'False | Bool
- | SBool | Bool -> Type
STrue | SBool 'True | Type
SFalse | SBool 'False | Type

### Bool と SBool が同型

`Bool -> SBool`, `SBool -> Bool` の関数が作れるのであれば `Bool` と `SBool` は同型。

```hs
fromSBool :: forall (a::Bool). SBool a -> Bool
fromSBool STrue  = True
fromSBool SFalse = False

-- この型では定義不可能 (SBool の a を消したい)
toSBool :: forall (a::Bool). Bool -> SBool a
toSBool True  = STrue   -- STrue  :: SBool 'True
toSBool False = SFalse  -- SFalse :: SBool 'False
```

この問題を解決するために存在型 `SomeSBool` を定義する

```hs
data SomeSBool where
  SomeSBool :: forall (a::Bool). SBool a -> SomeSBool

toSBool :: Bool -> SomeSBool
toSBool True  = SomeSBool STrue
toSBool False = SomeSBool SFalse
```

ここで `toSBool` と `fromSBool` の型を確認してみると

```hs
toSBool :: Bool -> SomeSBool
fromSBool :: forall (a::Bool). SBool a -> Bool
```

`SomeSBool` と `SBool a` という異なる型になっているので、補助関数 `withSomeSBool` を定義する。この関数は `Rank2` の関数である。

```hs
withSomeSBool :: SomeSBool -> (forall (a :: Bool). SBool a -> r) -> r
withSomeSBool (SomeSBool s) f = f s
```

この関数を組み合わせれば `Bool` 値を型レベルに Promote し、計算した結果を Demote して返す関数 `matchBool` が定義できる。

```hs
matchBool :: (forall (a :: Bool). SBool a -> r) -> Bool -> r
matchBool f b = withSomeSBool (toSBool b) f
```

これは以下のように使う

```hs
λ> matchSBool fromSBool True
True
λ> matchSBool fromSBool False
False

--
showSBool :: forall (a::Bool). SBool a -> String
showSBool STrue  = "True"
showSBool SFalse = "False

λ> matchSBool showSBool True
"True"
λ> matchSBool showSBool False
"False"
```

## Maybe Bool を考えよう

```hs
data SMaybe :: Maybe Bool -> Type where
  SNothing :: SMaybe 'Nothing
  SJust    :: forall (a::Bool). SBool a -> SMaybe ('Just a)
```

ここでのポイントは `SJust` の `a::Bool` である。

値 | 型 | Kind
----|----|------
Nothing, Just True, Just False | Maybe Bool | Type
- | 'Nothing | Maybe a
- | 'Just | a -> Maybe a
- | 'Just 'True | Maybe Bool
- | 'Just 'False | Maybe Bool
SNothing | SMaybe 'Nothing | Type
SJust STrue | SMaybe ('Just 'True) | Type
SJust SFalse | SMaybe ('Just 'False) | Type

まずは、さきほどと同じく `fromSMaybe` を定義する。

`SJust a` の値 `a` の型は `a::Bool` なので、`fromSBool` が適用できる。

```hs
fromSMaybe :: forall (a::Maybe Bool). SMaybe a -> Maybe Bool
fromSMaybe SNothing  = Nothing
fromSMaybe (SJust a) = Just (fromSBool a)
```

具体的には以下のように使う。

```hs
λ> fromSMaybe SNothing
Nothing
λ> fromSMaybe (SJust STrue)
Just True
λ> fromSMaybe (SJust SFalse)
Just False
λ> fromSMaybe (SJust False)

<interactive>:37:19: error:
    • Couldn't match expected type ‘SBool a0’ with actual type ‘Bool’
    • In the first argument of ‘SJust’, namely ‘False’
      In the first argument of ‘fromSMaybe’, namely ‘(SJust False)’
      In the expression: fromSMaybe (SJust False)
```

次に `SomeSMaybe` を定義する。

```hs
data SomeSMaybe where
  SomeSMaybe :: forall (a::Maybe Bool). SMaybe a -> SomeSMaybe
```

ここで `toSMaybe` を以下のように定義したくなるかもしれない

```hs
toSMaybe :: Maybe Bool -> SomeSMaybe
toSMaybe Nothing = SomeSMaybe SNothing
toSMaybe (Just a) = SomeSMaybe (SJust $ toSBool a)
```

しかし、これはうまく行かない。

なぜなら `toSBool a` の型は `SomeSBool` であり、`SJust` が期待する `SBool a` ではないからである。

そのため `toSMaybe` の正しい定義は以下のようになる。

```hs
toSMaybe :: Maybe Bool -> SomeSMaybe
toSMaybe Nothing = SomeSMaybe SNothing
toSMaybe (Just a) = withSomeSBool (toSBool a) (SomeSMaybe . SJust)
```

最後に `withSomeSMaybe` を定義する。

```hs
withSomeSMaybe :: SomeSMaybe -> (forall (a :: Maybe Bool). SMaybe a -> r) -> r
withSomeSMaybe (SomeSMaybe s) f = f s
```

実際に動作確認してみよう。

```hs
λ> withSomeSMaybe (toSMaybe $ Just True) fromSMaybe
Just True

λ> withSomeSMaybe (toSMaybe $ Just False) fromSMaybe
Just False

λ> withSomeSMaybe (toSMaybe Nothing) fromSMaybe
Nothing
```

## 15.3 抽象化しよう

`Bool` や `Maybe Bool` の例のように個別具体的に

- `S_XXX`
- `fromS_XXX`
- `SomeS_XXX`
- `toS_XXX`
- `withSomeS_XXX`

のような関数を定義するのも良いかもしれないが、これらは非常によく似た定義になっているため、抽象化できそうだ。

まずは完全な定義を示す。

```hs
data family Sing (a::k)

data SomeSing :: Type -> Type where
  SomeSing :: forall (a::k). Sing a -> SomeSing k

withSomeSing :: SomeSing k -> (forall (a::k). Sing a -> r) -> r
withSomeSing (SomeSing s) f = f s

class SingKind k where
  type Demote k = r | r -> k
  toSing   :: Demote k -> SomeSing k
  fromSing :: Sing (a::k) -> Demote k
```

### (1) Sing 型

```hs
data SBool (a :: Bool) where
  STrue  :: SBool 'True
  SFalse :: SBool 'False

data SMaybe :: (a :: Maybe Bool) where
  SNothing :: SMaybe 'Nothing
  SJust    :: forall (a::Bool). SBool a -> SMaybe ('Just a)
```

具体例の2つを見比べたときに異なる部分というのは `a` のカインドとデータコンストラクタだけなので、以下のように抽象化することは正しそう。

```hs
data family Sing (a::k)
```

### (2) SomeSing 型

ここはかなり大事

```hs
data SomeSBool where
  SomeSBool :: forall (a::Bool). SBool a -> SomeSBool

data SomeSMaybe where
  SomeSMaybe :: forall (a::Maybe Bool). SMaybe a -> SomeSMaybe
```

上記の定義を素朴に抽象化すれば以下のような定義になりそう。

```hs
data SomeSing where
  SomeSing :: forall (a::k). Sing a -> SomeSing
```

しかし、これでは `k` の制約が無いため上手くいかない。

その解決策として、以下のように `k` を `SomeSing` に持たせておけば `(a::k)` の `k` を決定できる。

```hs
data SomeSing :: Type -> Type where
  SomeSing :: forall (a::k). Sing a -> SomeSing k
```

### (3) withSomeSing 関数

```hs
withSomeSBool :: SomeSBool -> (forall (a :: Bool). SBool a -> r) -> r
withSomeSBool (SomeSBool s) f = f s

withSomeSMaybe :: SomeSMaybe -> (forall (a :: Maybe Bool). SMaybe a -> r) -> r
withSomeSMaybe (SomeSMaybe s) f = f s
```

これは自明です。

```hs
withSomeSing :: SomeSing k -> (forall (a::k). Sing a -> r) -> r
withSomeSing (SomeSing s) f = f s
```

### (4) SingKind 型クラス

```hs
toSBool :: Bool -> SomeSBool
toSBool True  = SomeSBool STrue
toSBool False = SomeSBool SFalse

fromSBool :: forall (a::Bool). SBool a -> Bool
fromSBool STrue  = True
fromSBool SFalse = False

toSMaybe :: Maybe Bool -> SomeSMaybe
toSMaybe Nothing = SomeSMaybe SNothing
toSMaybe (Just a) = withSomeSBool (toSBool a) (SomeSMaybe . SJust)

fromSMaybe :: forall (a::Maybe Bool). SMaybe a -> Maybe Bool
fromSMaybe SNothing  = Nothing
fromSMaybe (SJust a) = Just (fromSBool a)
```

同型であることを保証するために `toXXX`, `fromXXX` を持つ型クラスに抽象化することは自然。

また、定義を見ればわかるとおり `Bool` や `Maybe Bool` などの昇格する前の型情報も必要となるため、`Demote k` が追加されている。(この型はそれぞれ異なるため型族になっている)。デフォルト実装は用意できそうにない。

```hs
class SingKind k where
  type Demote k = r | r -> k
  toSing   :: Demote k    -> SomeSing k
  fromSing :: Sing (a::k) -> Demote k
```

## 15.3 SingI

<iframe style="border: none;" width="800" height="450" src="https://www.figma.com/embed?embed_host=share&url=https%3A%2F%2Fwww.figma.com%2Ffile%2FAgquwTzP53l6rMGqvqNRjV%2FCh15%3Fnode-id%3D64%253A2" allowfullscreen></iframe>

`SingI` は 15.3 で定義されたが、実はまだ利用していない。これは15.4節のための布石である。

```hs
class SingI (a::k) where
  sing :: Sing a

instance SingI 'True where
  sing = STrue

instance SingI 'False where
  sing = SFalse
```

この型クラスが値を持たない型とシングルトンを繋ぐ。

## 前半まとめ

抽象化された定義を使えば以下のようになる。

値 | 型 | Kind
----|----|------
True, False | Bool | Type
- | 'True | Bool
- | 'False | Bool
STrue | Sing 'True | Type
SFalse | Sing 'False | Type

値 | 型 | Kind
----|----|------
Nothing, Just True, Just False | Maybe Bool | Type
- | 'Nothing | Maybe a
- | 'Just 'True | Maybe Bool
- | 'Just 'False | Maybe Bool
SNothing | Sing 'Nothing | Type
SJust STrue | Sing ('Just 'True) | Type
SJust SFalse | Sing ('Just 'False) | Type

## 15.4 singletons が生成するもの

- Haskell のデータ型
- Sing データ型
- SingKind 型クラスのインスタンス
- SIngI 型クラスのインスタンス
- SDecide 型クラスのインスタンス

### 15.4-i

間違った定義

```hs
instance SDecide a => SDecide (Maybe a) where
  SNothing %~ SNothing = Proved Refl
  SJust a %~ SJust b = a %~ b
  _ %~ _ = Disproved $ const undefined
```

`a %~ b` の結果をそのまま返しても `a` と `b` が等しいことが型レベルで記録されるだけで、`SJust a` と `SJust b` が正しいということにはならない。

そのため、正しい定義では、値としては全く同じ `Proved Refl` を返すことで、異なる型を返すようにしている。

正しい定義

```hs
instance SDecide a => SDecide (Maybe a) where
  SNothing %~ SNothing = Proved Refl
  SJust a %~ SJust b =
    case a %~ b of
      Proved Refl -> Proved Refl
      Disproved _ -> Disproved $ const undefined
  _ %~ _ = Disproved $ const undefined
```

-----

詳しい解説

```hs
class SDecide k where
  (%~) :: SIng (a::k) -> Sing (b::k) -> Decision (a :~: b)
```

`k = Maybe Bool` として考えましょう。

```hs
(%~) :: SIng (a::Maybe Bool) -> Sing (b::Maybe Bool) -> Decision (a :~: b)
```

この時 `a` や `b` の型というのは以下のどれかになります。また、これらの型はシングルトンなので、対応する値は必ず1つです。

値 | 型
-------|-------
`SNothing` | `Sing ('Nothing)`
`SJust STrue` | `Sing ('Just 'True)`
`SJust SFalse` | `Sing ('Just 'False)`

誤った定義では何が起きているのでしょうか？

```hs
instance SDecide (Maybe Bool) where
  SNothing %~ SNothing = Proved Refl
  SJust a %~ SJust b = a %~ b
  _ %~ _ = Disproved $ const undefined
```

この時 `SJust a` と `SJust b` のそれぞれの `a`, `b` の値は上記の表より `STrue` か `SFalse` です。

その値によって再帰的に `%~` が呼ばれます。結果の値と型は当然、以下の4種類のどれかです。

値 | 型 | 結果の値と型
----|-----|------
`STrue  %~ STrue`  | `'True  :~: 'True`  | `Proved Refl :: Decision ('True :~: 'True)`
`STrue  %~ SFalse` | `'True  :~: 'False` | `Disproved (const undefined) :: Decision a`
`SFalse %~ STrue`  | `'False :~: 'True`  | `Disproved (const undefined) :: Decision a`
`SFalse %~ SFalse` | `'False :~: 'False` | `Proved Refl :: Decision ('False :~: 'False)`

```hs
λ> :t Proved (Refl :: True :~: True)
Proved (Refl :: True :~: True) :: Decision ('True :~: 'True)

λ> :t Disproved (const undefined)
Disproved (const undefined) :: Decision b
```

ということは結果として期待している

- `'SJust 'True :~: 'SJust 'True`
- `'SJust 'False :~: 'SJust 'False`
- `'SNothing :~: SNothing`

のどれにも当てはまらない型が返ってくることになってしまいます。

このことが原因でコンパイルエラーとなります。
