{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module HFlint.FMPQ.Algebra
where

import qualified Prelude as P
import Prelude ( (.), ($), (/=)
               , Bool(..), otherwise
               ,  const, flip, Integer
               )
import Data.Maybe
import Numeric.Algebra
import Numeric.Decidable.Units
import Numeric.Decidable.Zero
import Numeric.Semiring.Integral

import HFlint.Internal.Flint
import HFlint.FMPQ.Arithmetic ()
import HFlint.FMPQ.FFI
import HFlint.FMPQ.Internal ()


instance Additive FMPQ where
  (+) = lift2Flint_ $ const fmpq_add

instance Abelian FMPQ

instance Group FMPQ where
  (-) = lift2Flint_ $ const fmpq_sub
  negate = liftFlint_ $ const fmpq_neg
  subtract = flip (-)
  times n a = P.fromIntegral n P.* a


instance LeftModule Integer FMPQ where
  a .* b = P.fromInteger a P.* b

instance RightModule Integer FMPQ where
  a *. b = a P.* P.fromInteger b

instance LeftModule Natural FMPQ where
  a .* b = P.fromIntegral a P.* b

instance RightModule Natural FMPQ where
  a *. b = a P.* P.fromIntegral b

instance Monoidal FMPQ where
  zero = lift0FlintWithType_ FMPQType $ const fmpq_zero

instance DecidableZero FMPQ where
  isZero = (0/=) . liftFlint0 (const fmpq_is_zero)


instance Multiplicative FMPQ where
  (*) = lift2Flint_ $ const fmpq_mul

instance Commutative FMPQ

instance Unital FMPQ where
  one = lift0FlintWithType_ FMPQType $ const fmpq_one

instance DecidableUnits FMPQ where
  isUnit = const True
  recipUnit a = Just $ recip a


instance Rig FMPQ where
  fromNatural = P.fromIntegral

instance Semiring FMPQ
instance IntegralSemiring FMPQ
instance Ring FMPQ where
  fromInteger = P.fromInteger

instance Division FMPQ where
  recip = P.recip
  (/) = (P./)
  (\\) = flip (P./)
