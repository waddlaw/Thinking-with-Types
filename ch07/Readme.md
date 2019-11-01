# ch07. Existential Types

## Exercise 7.1-i

`forall a. a -> r` の関数ではできることが少ないので面白く無い。

```haskell
λ> elimAny (const 0) (Any 10)
0
```

## Exercise 7.1-ii

コンパイルが通らなくなる。`Show`のクラス制約を満たせないため

```hs
ch07/Ch07.hs:18:36: error:
    • No instance for (Show t) arising from a use of ‘show’
      Possible fix:
        add (Show t) to the context of the data constructor ‘HasShow’
    • In the second argument of ‘(++)’, namely ‘show s’
      In the expression: "HasShow " ++ show s
      In an equation for ‘show’: show (HasShow s) = "HasShow " ++ show s
   |
18 |   show (HasShow s) = "HasShow " ++ show s
   |                                    ^^^^^^
Failed, no modules loaded.
```

## Exercise 7.1-iii

問題の意味がわからない。

```hs
instance Show HasShow where
  show (HasShow s) = show s
```

模範解答

```hs
instance Show HasShow where
  show = elimHasShow show
```
