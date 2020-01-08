{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module DepPairs where

import Data.Aeson
import Data.Constraint
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Singletons.Prelude
import Data.Singletons.TH

data Sigma (f :: k -> Type) where
  Sigma :: Sing a -> f a -> Sigma f

withSigma
  :: (forall (a :: k). Sing a -> f a -> r)
  -> Sigma f
  -> r
withSigma c (Sigma s f) = c s f

toSigma :: SingI a => f a -> Sigma f
toSigma fa = Sigma sing fa

{-
λ> sing :: Sing True
STrue

λ> :t toSigma (SJust STrue)
toSigma (SJust STrue) :: Sigma Sing

λ> :t toSigma (Proxy @True)
toSigma (Proxy @True) :: Sigma Proxy

λ> :t toSigma (Nothing @Maybe)
<interactive>:1:19: error:
-}

fromSigma
  :: forall k (a::k) (f::k -> Type)
   . (SingI a, SDecide k)
  => Sigma f
  -> Maybe (f a)
fromSigma (Sigma s f) =
  case s %~ sing @a of
    Proved Refl -> Just f
    Disproved _ -> Nothing

class Dict1 c (f :: k -> Type) where
  dict1 :: Sing (a::k) -> Dict (c (f a))

instance
  ( Dict1 Eq (f :: k -> Type)
  , SDecide k
  ) => Eq (Sigma f) where
  Sigma sa fa == Sigma sb fb =
    case sa %~ sb of
      Proved Refl ->
        case dict1 @Eq @f sa of
          Dict -> fa == fb
      Disproved _ -> False

instance
  ( Dict1 Show (f :: k -> Type)
  , Show (Demote k)
  , SingKind k
  ) => Show (Sigma f) where
  show (Sigma sa fa) =
    case dict1 @Show @f sa of
      Dict -> mconcat
        [ "Sigma "
        , show $ fromSing sa
        , " ("
        , show fa
        , ")"
        ]

instance Dict1 Show Proxy where
  dict1 _ = Dict

-- instance Dict1 Show Sing where
--   dict1 _ = Dict

-- instance Show (Sing a) where
--   show _ = "Sing"

{-
fromSing :: SingKind k => Sing (a :: k) -> Demote k

λ> fromSing STrue
True

λ> toSigma (Proxy @True)
Sigma True (Proxy)

λ> toSigma (SJust STrue)
Sigma Just True (Sing)
-}

-- instance
--   ( Dict1 Eq (f :: k -> Type)
--   , SDecide k
--   ) => Eq (Sigma f) where
--   Sigma sa fa == Sigma sb fb =
--     case sa %~ sb of
--       Proved Refl ->
--         case dict1 @Eq @f sa of
--           Dict -> fa == fb
--       Disproved _ -> False

-- ex15-5.i
instance
  ( Dict1 Eq (f :: k -> Type)
  , Dict1 Ord f
  , SDecide k
  , Ord (Demote k)
  , SingKind k
  ) => Ord (Sigma f) where
  compare (Sigma sa fa) (Sigma sb fb) =
    case sa %~ sb of
      Proved Refl ->
        case dict1 @Ord @f sa of
          Dict -> compare fa fb
      Disproved _ -> compare (fromSing sa) (fromSing sb)
      -- 型レベルに順序は無いので term に飛ばしている

singletons [d|
  data LogType
    = JsonMsg
    | TextMsg
    deriving (Eq, Ord, Show)
  |]

data family LogMsg (msg :: LogType)

data instance LogMsg 'JsonMsg = Json Value
  deriving (Eq, Show)

data instance LogMsg 'TextMsg = Text String
  deriving (Eq, Show)

instance ( c (LogMsg 'JsonMsg)
         , c (LogMsg 'TextMsg)
         ) => Dict1 c LogMsg where
  dict1 SJsonMsg = Dict
  dict1 STextMsg = Dict

logs :: [Sigma LogMsg]
logs =
  [ toSigma $ Text "hello"
  , toSigma $ Json $
      object
        [ "world" .= (5 :: Int) ]
  , toSigma $ Text "structured logging is cool"
  ]

showLogs :: [Sigma LogMsg] -> [String]
showLogs = fmap $ withSigma $ \sa fa ->
  case dict1 @Show @LogMsg sa of
    Dict -> show fa

catSigmas
  :: forall k (a::k) f
   . (SingI a, SDecide k)
   => [Sigma f]
   -> [f a]
catSigmas = mapMaybe fromSigma

jsonLogs :: [LogMsg 'JsonMsg]
jsonLogs = catSigmas logs

textLogs :: [LogMsg 'TextMsg]
textLogs = catSigmas logs

anyLogs :: forall (a :: LogType). SingI a => [LogMsg a]
anyLogs = catSigmas logs