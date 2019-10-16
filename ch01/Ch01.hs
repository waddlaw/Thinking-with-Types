{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Ch01 where

import Prelude hiding (Bool(..))
import Data.Proxy
import Data.Word
import Data.Void

data Bool
  = False
  | True
  deriving (Enum, Bounded)

class Cardinality a where
  cardinality :: Proxy a -> Int

instance Cardinality Void where
  cardinality _ = 0

instance Cardinality () where
  cardinality _ = forallElems (Proxy @())

instance Cardinality Bool where
  cardinality _ = forallElems (Proxy @Bool)

instance (Cardinality a, Cardinality b) => Cardinality (Either a b) where
  cardinality _ = cardinality (Proxy @a) + cardinality (Proxy @b)

instance Cardinality a => Cardinality [a] where
  cardinality _ = 1 + cardinality (Proxy @a) + cardinality (Proxy @[a])

forallElems :: (Bounded a, Enum a) => Proxy a -> Int
forallElems (_:: Proxy a) = length ([minBound .. maxBound] :: [a])

class Iso s t where
  to   :: s -> t
  from :: t -> s

data Spin
  = Up
  | Down

boolToSpin1 :: Bool -> Spin
boolToSpin1 False = Up
boolToSpin1 True  = Down

spinToBool1 :: Spin -> Bool
spinToBool1 Up   = False
spinToBool1 Down = True

boolToSpin2 :: Bool -> Spin
boolToSpin2 False = Down
boolToSpin2 True  = Up

spinToBool2 :: Spin -> Bool
spinToBool2 Up   = True
spinToBool2 Down = False

data Deal a b
  = This a
  | That b
  | TheOther Bool

instance (Cardinality a, Cardinality b) => Cardinality (Deal a b) where
  cardinality _ = cardinality (Proxy @a) + cardinality (Proxy @b) + cardinality (Proxy @Bool)

instance Cardinality a => Cardinality (Maybe a) where
  cardinality _ = 1 + cardinality (Proxy @a)

instance (Cardinality a, Cardinality b) => Cardinality (a, b) where
  cardinality _ = cardinality (Proxy @a) * cardinality (Proxy @b)

data MixedFraction a = Fraction
  { mixedBit    :: Word8
  , numerator   :: a
  , denominator :: a
  }

instance Cardinality Word8 where
  cardinality _ = 256

instance Cardinality a => Cardinality (MixedFraction a) where
  cardinality _ = cardinality (Proxy @Word8) * cardinality (Proxy @a) * cardinality (Proxy @a)

prodUnitTo :: a -> (a, ())
prodUnitTo a = (a, ())

prodUnitFrom :: (a, ()) -> a
prodUnitFrom (a, ()) = a

sumUnitTo :: Either a Void -> a
sumUnitTo (Left a)  = a
sumUnitTo (Right v) = absurd v

sumUnitFrom :: a -> Either a Void
sumUnitFrom = Left

instance (Cardinality a, Cardinality b) => Cardinality ((->) a b) where
  cardinality _ = cardinality (Proxy @a) ^ cardinality (Proxy @b)

data TicTacToe a = TicTacToe
  { topLeft   :: a
  , topCenter :: a
  , topRight  :: a
  , midLeft   :: a
  , midCenter :: a
  , midRight  :: a
  , botLeft   :: a
  , botCenter :: a
  , botRight  :: a
  }

emptyBoard :: TicTacToe (Maybe Bool)
emptyBoard =
  TicTacToe
    Nothing Nothing Nothing
    Nothing Nothing Nothing
    Nothing Nothing Nothing

data Three = One | Two | Three
  deriving (Eq, Ord, Enum, Bounded)

data TicTacToe2 a = TicTacToe2
  { board :: Three -> Three -> a
  }

emptyBoard2 :: TicTacToe2 (Maybe Bool)
emptyBoard2 =
  TicTacToe2 $ const $ const Nothing