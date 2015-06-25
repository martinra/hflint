{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving
  , TemplateHaskell
  #-}

module HFlint.FMPQ.Algebra
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
-- import qualified Prelude as P

import Math.Structure.Additive
import Math.Structure.Instances.TH.Additive
import Math.Structure.Instances.TH.Multiplicative
import Math.Structure.Instances.TH.Ring
import Math.Structure.Multiplicative

import HFlint.FMPQ.Arithmetic ()
import HFlint.FMPQ.FFI

mkAbelianGroupInstanceFromNum ''FMPQ
mkCommutativeGroupInstanceFromNonZeroFractional ''FMPQ
mkFieldInstance ''FMPQ

instance DecidableZero FMPQ where
  isZero = (==0)

instance DecidableOne FMPQ where
  isOne = (==1)
