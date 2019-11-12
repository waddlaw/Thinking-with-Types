{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Ch09 where

import Data.Kind (Type)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import GHC.TypeLits

data (a :: k1) :<< (b :: k2)
infixr 5 :<<

class HasPrintf a where
  type Printf a :: Type
  format :: String -> Proxy a -> Printf a

instance KnownSymbol text => HasPrintf (text :: Symbol) where
  type Printf text = String
  format :: String -> Proxy (text :: Symbol) -> Printf (text :: Symbol)
  -- format :: String -> Proxy (text :: Symbol) -> String
  format s _ = s <> symbolVal (Proxy @text)

instance (KnownSymbol text, HasPrintf a) => HasPrintf ((text :: Symbol) :<< a) where
  type Printf (text :<< a) = Printf a
  format :: String -> Proxy ((text :: Symbol) :<< a) -> Printf ((text :: Symbol) :<< a)
  -- format :: String -> Proxy ((text :: Symbol) :<< a) -> Printf a
  format s _ = format (s <> symbolVal (Proxy @text)) (Proxy @a)

instance (Show param, HasPrintf a) => HasPrintf ((param :: Type) :<< a) where
  type Printf (param :<< a) = param -> Printf a
  format :: String -> Proxy ((param :: Type) :<< a) -> Printf ((param :: Type) :<< a)
  -- format :: String -> Proxy ((param :: Type) :<< a) -> param -> Printf a
  format s _ v = format (s <> show v) (Proxy @a)

instance {-# OVERLAPPING #-} HasPrintf a => HasPrintf (String :<< a) where
  type Printf (String :<< a) = String -> Printf a
  format :: String -> Proxy (String :<< a) -> Printf (String :<< a)
  -- format :: String -> Proxy (String :<< a) -> String -> Printf a
  format s _ v = format (s <> v) (Proxy @a)

{-
λ> :k! Printf (Int :<< ":" :<< Bool)
Printf (Int :<< ":" :<< Bool) :: Type
= Int -> Printf Bool
-}

printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""