{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module CalcRank where

import Data.List
import Test.Hspec

data Ty = Ty Forall TyBody
  deriving (Eq, Show)

data TyBody
  = TyPrim TyName
  | TyVar TyName
  | TyApp Ty [Ty]
  | TyArrow Ty Ty
  deriving (Eq, Show)

newtype Forall = Forall [VarName]
  deriving (Eq, Show)

type TyName  = String
type VarName = String

calcRank :: Ty -> Int
calcRank (Ty (Forall tyVars) body) = case body of
  TyPrim{} -> 0
  TyVar{}  -> baseRank
  TyApp{}  -> 0
  TyArrow ty1 ty2 ->
    let rankL = calcRank ty1
        rankR = calcRank ty2
        isHRFunc = isHRT rankL || isHRT rankR -- 関数の引数と戻り値に高階ランク型が含まれているかチェック
        funcRank = if isHRFunc then 0 else baseRank
    in funcRank + max (update rankL) rankR
  where
    isHRT rank = rank > 0
    hasForall = not (null tyVars)
    baseRank = if hasForall then 1 else 0
    update rank = if isHRT rank then rank+1 else 0

class Pretty a where
  pretty :: a -> String

instance Pretty Ty where
  pretty :: Ty -> String
  pretty (Ty forall body) = pretty forall <> pretty body

instance Pretty TyBody where
  pretty :: TyBody -> String
  pretty = \case
    TyPrim name -> name
    TyVar name -> name
    TyApp ty1 [] -> ""
    TyApp ty1 [ty2] -> pretty ty1 <> " " <> pretty ty2
    TyApp ty1 tys -> pretty ty1 <> " " <> "(" <> (concat . intersperse " " . map pretty $ tys) <> ")"
    TyArrow ty1@(Ty _ TyArrow{}) ty2 -> "(" <> pretty ty1 <> ")" <> " -> " <> pretty ty2
    TyArrow ty1 ty2 -> pretty ty1 <> " -> " <> pretty ty2

instance Pretty Forall where
  pretty :: Forall -> String
  pretty (Forall []) = ""
  pretty (Forall l@(x:xs)) = "forall " <> concat (intersperse " " l) <> ". "

-- examples
-- Int -> Int
rank0 :: Ty
rank0 = mkTyArrow ty ty
  where
    ty = Ty (Forall []) $ TyPrim "Int"

-- forall a. a -> a
rank1 :: Ty
rank1 = Ty (Forall ["a"]) $ TyArrow ty ty
  where
    ty = mkTyVar "a"

-- Int -> forall a. a -> a
rank1_1 :: Ty
rank1_1 = mkTyArrow ty1 ty2
  where
    ty1  = Ty (Forall []) $ TyPrim "Int"
    ty2  = Ty (Forall ["a"]) $ TyArrow ty21 ty21
    ty21 = mkTyVar "a"

-- forall a. (a -> a) -> Int
rank1_2 :: Ty
rank1_2 = Ty (Forall ["a"]) $ TyArrow ty1 (mkTyPrim "Int")
  where
    ty1 = mkTyArrow' (Forall []) (mkTyVar "a") (mkTyVar "a")

-- forall a b. (a -> b) -> (forall c. c -> a) -> b
rank2 :: Ty
rank2 = Ty (Forall ["a", "b"]) $ TyArrow ty1 ty2
  where
    ty1 = mkTyArrow (mkTyVar "a") (mkTyVar "b")
    ty2 = mkTyArrow ty21 ty22
    ty21 = mkTyArrow' (Forall ["c"]) (mkTyVar "c") (mkTyVar "a")
    ty22 = mkTyVar "b"

-- (forall a. a -> a) -> Int
rank2_1 :: Ty
rank2_1 = mkTyArrow ty1 (mkTyPrim "Int")
  where
    ty1 = mkTyArrow' (Forall ["a"]) (mkTyVar "a") (mkTyVar "a")

-- (forall a. a -> a) -> (forall b. b -> b)
rank2_2 :: Ty
rank2_2 = mkTyArrow ty1 ty2
  where
    ty1 = mkTyArrow' (Forall ["a"]) (mkTyVar "a") (mkTyVar "a")
    ty2 = mkTyArrow' (Forall ["b"]) (mkTyVar "b") (mkTyVar "b")

-- forall m a b z. ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a
rank3 :: Ty
rank3 = mkTyArrow' (Forall ["m", "a", "b", "z"]) ty1 ty2
  where
    ty1 = mkTyArrow ty11 ty12
    ty11 = mkTyArrow' (Forall ["x"]) ty111 ty112
    ty111 = mkTyApp (mkTyVar "m") [mkTyVar "x"]
    ty112 = mkTyApp (mkTyVar "b") [mkTyVar "z", mkTyVar "m", mkTyVar "x"]
    ty12 = mkTyApp (mkTyVar "b") [mkTyVar "z", mkTyVar "m", mkTyVar "a"]
    ty2 = mkTyApp (mkTyVar "m") [mkTyVar "a"]

-- ((forall a. a->a) -> Int) -> Bool -> Bool
rank3_1 :: Ty
rank3_1 = mkTyArrow ty2 ty3
  where
    ty3 = mkTyArrow (mkTyPrim "Bool") (mkTyPrim "Bool")
    ty2 = mkTyArrow ty1 (mkTyPrim "Int")
    ty1 = mkTyArrow' (Forall ["a"]) (mkTyVar "a") (mkTyVar "a")

-- constructors
mkTyArrow :: Ty -> Ty -> Ty
mkTyArrow = mkTyArrow' (Forall [])

mkTyArrow' :: Forall -> Ty -> Ty -> Ty
mkTyArrow' forall ty1 ty2 = Ty forall (TyArrow ty1 ty2)

mkTyApp :: Ty -> [Ty] -> Ty
mkTyApp ty1 tys = Ty (Forall []) (TyApp ty1 tys)

mkTyVar :: String -> Ty
mkTyVar x = Ty (Forall []) (TyVar x)

mkTyPrim :: String -> Ty
mkTyPrim x = Ty (Forall []) (TyPrim x)

-- TEST
test :: IO ()
test = hspec $ do
  describe "pretty print" $ do
    it "rank0"   $ pretty rank0   `shouldBe` "Int -> Int"
    it "rank1"   $ pretty rank1   `shouldBe` "forall a. a -> a"
    it "rank1_1" $ pretty rank1_1 `shouldBe` "Int -> forall a. a -> a"
    it "rank1_2" $ pretty rank1_2 `shouldBe` "forall a. (a -> a) -> Int"
    it "rank2"   $ pretty rank2   `shouldBe` "forall a b. (a -> b) -> (forall c. c -> a) -> b"
    it "rank2_1" $ pretty rank2_1 `shouldBe` "(forall a. a -> a) -> Int"
    it "rank2_2" $ pretty rank2_2 `shouldBe` "(forall a. a -> a) -> forall b. b -> b"
    it "rank3"   $ pretty rank3   `shouldBe` "forall m a b z. ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a"
    it "rank3_1" $ pretty rank3_1 `shouldBe` "((forall a. a -> a) -> Int) -> Bool -> Bool"

  describe "calc rank" $ do
    it "rank0"   $ calcRank rank0   `shouldBe` 0
    it "rank1"   $ calcRank rank1   `shouldBe` 1
    it "rank1_1" $ calcRank rank1_1 `shouldBe` 1
    it "rank1_2" $ calcRank rank1_2 `shouldBe` 1
    it "rank2"   $ calcRank rank2   `shouldBe` 2
    it "rank2_1" $ calcRank rank2_1 `shouldBe` 2
    it "rank2_2" $ calcRank rank2_2 `shouldBe` 2
    it "rank3"   $ calcRank rank3   `shouldBe` 3
    it "rank3_1" $ calcRank rank3_1 `shouldBe` 3