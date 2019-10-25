module Expr where

data Expr a
  = LitInt Int
  | LitBool Bool
  | Add (Expr a) (Expr a)
  | Not (Expr a)
  | If (Expr a) (Expr a) (Expr a)
  deriving Show

evalExpr :: Expr a ->  Expr a
evalExpr (LitInt i) = LitInt i
evalExpr (LitBool b) = LitBool b
evalExpr (Add e1 e2) =
  let LitInt x = evalExpr e1
      LitInt y = evalExpr e2
  in  LitInt (x + y)
evalExpr (Not e) =
  let LitBool x = evalExpr e
  in  LitBool (not x)
evalExpr (If e1 e2 e3) =
  let LitBool b = evalExpr e1
  in if b then evalExpr e2 else evalExpr e3

{--
λ> evalExpr . If (LitBool False) (LitInt 1) . Add (LitInt 5) $ LitInt 13
LitInt 18
λ> evalExpr . Not $ LitBool True 
LitBool False

λ> :t Not (LitInt 10)
Not (LitInt 10) :: Expr a
λ> evalExpr . Not $ LitInt 10
LitBool *** Exception: ch05/Expr.hs:19:7-28: Non-exhaustive patterns in LitBool x
--}