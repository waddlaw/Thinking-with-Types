{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Ch12 where

import GHC.TypeLits

instance
  ( TypeError
      ( Text "Attempting to interpret a number as a function "
   :$$: Text "in the type `"
   :<>: ShowType (a -> b)
   :<>: Text "'"
   :$$: Text "Did you forget to specify the function you wanted?"
      )
  ) => Num (a -> b) where

{-
λ> 1 True

<interactive>:1:1: error:
    • Attempting to interpret a number as a function 
      in the type `Bool -> t'
      Did you forget to specify the function you wanted?
    • When checking the inferred type
        it :: forall t. (TypeError ...) => t
-}