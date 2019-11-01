# 5.1 Introduction

```haskell
λ> :k Show
Show :: Type -> Constraint

λ> :k (Int ~ Int)
(Int ~ Int) :: Constraint
λ> :k (Int ~ Bool)
(Int ~ Bool) :: Constraint
```

# 5.2 GADTs

素朴な `Expr` 型の問題点

```haskell
data Expr a
  = LitInt Int
  | LitBool Bool
  | Add (Expr a) (Expr a)
  | Not (Expr a)
  | If (Expr a) (Expr a) (Expr a)
```

- `evalExpr :: Expr a -> a` のような関数が書けない。評価結果として `Int` と `Bool` の両方を返したい
- `Not (LitInt 10) :: Expr a` のような不正な値が作れてしまう

# 5.3 HETEROGENEOUS LISTS

`HList` の `==` について

- 比較する2つの値の型が異なる (長さが異なる) 場合はコンパイルエラー
- 型が同一の場合に初めて検査される
