{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Ch04 where

import Data.Typeable

-- broken :: (a -> b) -> a -> b
-- broken f a = apply
--   where
--     apply :: b
--     apply = f a

working :: forall a b. (a ->b) -> a -> b
working f a = apply
  where
    apply :: b
    apply = f a

typeName :: forall a . Typeable a => String
typeName = show . typeRep $ Proxy @a

type family AlwaysUnit a where
  AlwaysUnit a = ()

f1 :: AlwaysUnit a -> a
f1 = undefined

f2 :: b -> AlwaysUnit a -> b
f2 = undefined

f3 :: Show a => AlwaysUnit a -> String
f3 = undefined