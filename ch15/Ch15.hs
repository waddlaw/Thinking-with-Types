{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-}
module Main (main) where

import Control.Monad.Trans.Writer
import Data.Constraint (Dict (..))
import Data.Foldable (for_)
import Data.Kind (Type)

data SBool (b :: Bool) where
  STrue  :: SBool 'True
  SFalse :: SBool 'False

fromSBool :: forall (b::Bool). SBool b -> Bool
fromSBool STrue = True
fromSBool SFalse = False

data SomeSBool where
  SomeSBool :: forall (b::Bool). SBool b -> SomeSBool

withSomeSBool :: SomeSBool -> (forall (b :: Bool). SBool b -> r) -> r
withSomeSBool (SomeSBool s) f = f s

toSBool :: Bool -> SomeSBool
toSBool True = SomeSBool STrue
toSBool False = SomeSBool SFalse

{-
λ> withSomeSBool (toSBool True) fromSBool 
True

λ> withSomeSBool (toSBool False) fromSBool 
False
-}

class Monad (LoggingMonad b) => MonadLogging (b :: Bool) where
  type LoggingMonad b = (r :: Type -> Type) | r -> b
  logMsg :: String -> LoggingMonad b ()
  runLogging :: LoggingMonad b a -> IO a

instance MonadLogging 'False where
  type LoggingMonad 'False = IO
  logMsg _ = pure ()
  runLogging = id

instance MonadLogging 'True where
  type LoggingMonad 'True = WriterT [String] IO
  logMsg s = tell [s]
  runLogging m = do
    (a,w) <- runWriterT m
    for_ w putStrLn
    pure a

program :: MonadLogging b => LoggingMonad b ()
program = do
  logMsg "hello world"
  pure ()

main :: IO ()
main = do
  bool <- read <$> getLine
  withSomeSBool (toSBool bool) $ \(sb :: SBool b) ->
    case dict @MonadLogging sb of
      Dict -> runLogging @b program

dict :: (c 'True, c 'False) => SBool b -> Dict (c b)
dict STrue = Dict
dict SFalse = Dict

{-
λ> main
True
hello world
λ> main
False
-}

data SMaybe :: Maybe Bool -> Type where
  SNothing :: SMaybe 'Nothing
  SJust    :: forall (a::Bool). SBool a -> SMaybe ('Just a)

{-
λ> fromSMaybe SNothing
Nothing

λ> fromSMaybe $ SJust STrue
Just True
-}
fromSMaybe :: forall (a::Maybe Bool). SMaybe a -> Maybe Bool
fromSMaybe SNothing  = Nothing
fromSMaybe (SJust a) = Just (fromSBool a)

data SomeSMaybe where
  SomeSMaybe :: forall (a::Maybe Bool). SMaybe a -> SomeSMaybe

withSomeSMaybe :: SomeSMaybe -> (forall (a :: Maybe Bool). SMaybe a -> r) -> r
withSomeSMaybe (SomeSMaybe s) f = f s

toSMaybe :: Maybe Bool -> SomeSMaybe
toSMaybe Nothing = SomeSMaybe SNothing
toSMaybe (Just a) = withSomeSBool (toSBool a) (SomeSMaybe . SJust)

{-
λ> withSomeSMaybe (toSMaybe $ Just True) fromSMaybe
Just True

λ> withSomeSMaybe (toSMaybe $ Just False) fromSMaybe
Just False

λ> withSomeSMaybe (toSMaybe Nothing) fromSMaybe
Nothing
-}