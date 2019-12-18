{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Linear where

import Control.Monad.Indexed
import Data.Coerce
import Fcf
import GHC.TypeLits (Nat)
import qualified GHC.TypeLits as TL
import Control.Monad.Indexed
import IxMonad
import Language.Haskell.DoNotation
import Prelude hiding (Monad(..), pure)
import qualified System.IO as SIO
import System.IO hiding (openFile, Handle)

data LinearState = LinearState
  { linearNextKey  :: Nat
  , linearOpenkeys :: [Nat]
  }

newtype Linear s (i :: LinearState) (j :: LinearState) a = Linear
  { unsafeRunLinear :: Ix IO i j a }
  deriving (IxFunctor, IxPointed, IxApplicative, IxMonad)

openFile :: FilePath -> IOMode -> Linear s ('LinearState next open)
                                           ('LinearState (next TL.+ 1) (next ': open))
                                           (Handle s next)
openFile = coerce SIO.openFile

newtype Handle s key = Handle { unsafeGetHandle :: SIO.Handle }

type IsOpen (key :: k) (ts :: [k]) = IsJust =<< Find (TyEq key) ts
type Close (key :: k) (ts :: [k]) = Filter (Not <=< TyEq key) ts

closeFile
  :: Eval (IsOpen key open) ~ 'True
  => Handle s key
  -> Linear s ('LinearState next open)
              ('LinearState next (Eval (Close key open)))
              ()
closeFile = coerce SIO.hClose

runLinear
  :: (forall s. Linear s ('LinearState 0 '[])
                         ('LinearState n '[])
                         a
     )
  -> IO a
runLinear = coerce

etcPasswd = openFile "passwd" ReadMode

{-
λ> :t runLinear (etcPasswd >>= closeFile)
runLinear (etcPasswd >>= closeFile) :: IO ()

λ> :t runLinear etcPasswd 
<interactive>:1:11: error:
    • Couldn't match type ‘'[0]’ with ‘'[]’
      Expected type: Linear s ('LinearState 0 '[]) ('LinearState 1 '[]) a
        Actual type: Linear
                       s ('LinearState 0 '[]) ('LinearState (0 TL.+ 1) '[0]) (Handle s 0)
    • In the first argument of ‘runLinear’, namely ‘etcPasswd’
      In the expression: runLinear etcPasswd

λ> :t runLinear (etcPasswd >>= \f -> closeFile f >> closeFile f)
<interactive>:1:47: error:
    • Couldn't match type ‘'False’ with ‘'True’
        arising from a use of ‘closeFile’
    • In the second argument of ‘(>>)’, namely ‘closeFile f’
      In the expression: closeFile f >> closeFile f
      In the second argument of ‘(>>=)’, namely
        ‘\ f -> closeFile f >> closeFile f’

λ> :t runLinear (etcPasswd >>= \f -> closeFile f >> pure f)
<interactive>:1:12: error:
    • Couldn't match type ‘a’ with ‘Handle s 0’
        because type variable ‘s’ would escape its scope
      This (rigid, skolem) type variable is bound by
        a type expected by the context:
          forall (s :: k0).
          Linear s ('LinearState 0 '[]) ('LinearState 1 '[]) a
        at <interactive>:1:1-53
      Expected type: Linear s ('LinearState 0 '[]) ('LinearState 1 '[]) a
        Actual type: Linear
                       s ('LinearState 0 '[]) ('LinearState 1 '[]) (Handle s 0)
    • In the first argument of ‘runLinear’, namely
        ‘(etcPasswd >>= \ f -> closeFile f >> pure f)’
      In the expression:
        runLinear (etcPasswd >>= \ f -> closeFile f >> pure f)
-}