{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RebindableSyntax #-}
module IxMonad where

import Control.Monad.Indexed
import Data.Coerce
import Language.Haskell.DoNotation
import Prelude hiding (Monad (..), pure)
import qualified Prelude (pure)

newtype Ix m i j a = Ix { unsafeRunIx :: m a }
  deriving (Functor, Applicative, Monad)

{-
instance Functor (Ix m i j) where
  fmap :: (a -> b) -> Ix m i j a -> Ix m i j b

instance Functor (Ix m i j) => Applicative (Ix m i j) where
  (<*>) :: Ix m i j (a -> b) -> Ix m i j a -> Ix m i j b
-}

{-
class IxFunctor f where
  imap :: (a -> b) -> f j k a -> f j k b
-}
instance Functor m => IxFunctor (Ix m) where
  imap = fmap

{-
class IxFunctor m => IxPointed m where
  ireturn :: a -> m i i a
-}
instance Applicative m => IxPointed (Ix m) where
  ireturn = Prelude.pure

{-
class IxPointed m => IxApplicative m where
  iap :: m i j (a -> b) -> m j k a -> m i k b
-}
instance Applicative m => IxApplicative (Ix m) where
  iap :: forall i j k a b
       . Ix m i j (a -> b)
      -> Ix m j k a
      -> Ix m i k b
  iap = coerce $ (<*>) @m @a @b
  -- (<*>) :: Ix m i j (a -> b) -> Ix m i j a -> Ix m i j b

instance Monad m => IxMonad (Ix m) where
  ibind :: forall i j k a b
         . (a -> Ix m j k b)
        -> Ix m i j a
        -> Ix m i k b
  ibind = coerce $ (=<<) @m @a @b