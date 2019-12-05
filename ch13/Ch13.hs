module Ch13 where

import GHC.Generics

data Foo a b c
  = F0
  | F1 a
  | F2 b c

instance (Eq a, Eq b, Eq c) => Eq (Foo a b c) where
  F0       == F0       = True
  F1 a1    == F1 a2    = a1 == a2
  F2 b1 c1 == F2 b2 c2 = b1 == b2 && c1 == c2

toCanonical :: Maybe a -> Either () a
toCanonical Nothing  = Left ()
toCanonical (Just a) = Right a

fromCanonical :: Either () a -> Maybe a
fromCanonical (Left ()) = Nothing
fromCanonical (Right a) = Just a

{-
data Meta = MetaData Symbol Symbol Symbol Bool
          | MetaCons Symbol FixityI Bool
          | MetaSel  (Maybe Symbol)
                     SourceUnpackedness SourceStrictness DecidedStrictness
-}