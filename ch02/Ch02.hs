{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module Ch02 where

import Data.Proxy

data UserType
  = NormalUser
  | Admin

data User = User { userAdminToken :: Maybe (Proxy 'Admin) }

doSensitiveThings :: Proxy 'Admin -> IO ()
doSensitiveThings = undefined

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True  y = 'True
  Or 'False y = y

{-
λ> :k! Or 'True 'True
Or 'True 'True :: Bool
= 'True
λ> :k! Or 'True 'False
Or 'True 'False :: Bool
= 'True
λ> :k! Or 'False 'True
Or 'False 'True :: Bool
= 'True
λ> :k! Or 'False 'False
Or 'False 'False :: Bool
= 'False
-}

type family Not (x :: Bool) :: Bool where
  Not 'True  = 'False
  Not 'False = 'True

{-
λ> :k! Not 'True
Not 'True :: Bool
= 'False
λ> :k! Not 'False
Not 'False :: Bool
= 'True
-}

type family Map (x :: a -> b) (i :: [a]) :: [b] where
  Map f '[]       = '[]
  Map f (x ': xs) = f x ': Map f xs

type family Foo (x :: Bool) (y :: Bool) :: Bool
type family Bar x y :: Bool -> Bool -> Bool