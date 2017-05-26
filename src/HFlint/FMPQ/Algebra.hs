{-# LANGUAGE
    TemplateHaskell
  #-}

module HFlint.FMPQ.Algebra
where

import Prelude ()
import HFlint.Utility.Prelude

import Math.Structure.Instances.TH.Additive
import Math.Structure.Instances.TH.Multiplicative
import Math.Structure.Instances.TH.Ring

import HFlint.FMPQ.Arithmetic ()
import HFlint.FMPQ.FFI
import HFlint.FMPZ
import HFlint.Internal.Lift


mkAbelianGroupInstanceFromNum (return []) [t|FMPQ|]
mkCommutativeGroupInstanceFromNonZeroFractional (return []) [t|FMPQ|]
mkFieldInstance (return []) [t|FMPQ|]

instance DecidableUnit FMPQ where
  isUnit = not . isZero
  toUnit = Unit

instance DecidableOne (Unit FMPQ) where
  isOne = isOne . fromUnit

instance MultiplicativeGroup (Unit FMPQ) where
  recip = Unit . fromNonZero . recip . NonZero . fromUnit


instance MultiplicativeSemigroupLeftAction FMPZ FMPQ where
  (*.) = flip (.*)
instance MultiplicativeLeftAction FMPZ FMPQ
instance LinearSemiringLeftAction FMPZ FMPQ
instance NonUnitalLeftModule FMPZ FMPQ
instance LeftModule FMPZ FMPQ

instance MultiplicativeSemigroupRightAction FMPZ FMPQ where
  (.*) = lift2Flint_ fmpq_mul_fmpz
instance MultiplicativeRightAction FMPZ FMPQ
instance LinearSemiringRightAction FMPZ FMPQ
instance NonUnitalRightModule FMPZ FMPQ
instance RightModule FMPZ FMPQ

instance NonUnitalModule FMPZ FMPQ
instance Module FMPZ FMPQ

instance SemiLeftAlgebra FMPZ FMPQ
instance NonUnitalLeftAlgebra FMPZ FMPQ
instance LeftAlgebra FMPZ FMPQ

instance SemiRightAlgebra FMPZ FMPQ
instance NonUnitalRightAlgebra FMPZ FMPQ
instance RightAlgebra FMPZ FMPQ

instance SemiAlgebra FMPZ FMPQ
instance NonUnitalAlgebra FMPZ FMPQ
instance Algebra FMPZ FMPQ
