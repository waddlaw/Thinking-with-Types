{-# LANGUAGE GADTs #-}
module Ch05 where

five :: Int
five = 5

five_ :: (a ~ Int) => a
five_ = 5

-- typesafe
data Expr a where
  LitInt :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Not :: Expr Bool -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

evalExpr :: Expr a -> a
evalExpr (LitInt i) = i
evalExpr (LitBool b) = b
evalExpr (Add x y) = evalExpr x + evalExpr y
evalExpr (Not x) = not $evalExpr x
evalExpr (If b x y) = if evalExpr b then evalExpr x else evalExpr y

{--
λ> evalExpr . If (LitBool False) (LitInt 1) . Add (LitInt 5) $ LitInt 13
18
λ> evalExpr . Not $ LitBool True 
False

λ> :t Not (LitInt 10)
<interactive>:1:6: error:
    • Couldn't match type ‘Int’ with ‘Bool’
      Expected type: Expr Bool
        Actual type: Expr Int
    • In the first argument of ‘Not’, namely ‘(LitInt 10)’
      In the expression: Not (LitInt 10)
--}

data Expr_ a
  = (a ~ Int)  => LitInt_ Int
  | (a ~ Bool) => LitBool_ Bool
  | (a ~ Int)  => Add_ (Expr_ Int) (Expr_ Int)
  | (a ~ Bool) => Not_ (Expr_ Bool)
  | If_ (Expr_ Bool) (Expr_ a) (Expr_ a)

{-
λ> :t LitInt
LitInt :: Int -> Expr Int
λ> :t LitInt_
LitInt_ :: Int -> Expr_ Int
-}
