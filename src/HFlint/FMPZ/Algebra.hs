{-# LANGUAGE
    TemplateHaskell
  #-}

module HFlint.FMPZ.Algebra
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

import HFlint.FMPZ.Arithmetic ()
import HFlint.FMPZ.FFI


mkAbelianGroupInstanceFromNum ''FMPZ
mkCommutativeMonoidInstanceFromNum ''FMPZ
mkEuclideanDomainInstanceFromIntegral ''FMPZ

instance DecidableZero FMPZ where
  isZero = (==0)

instance DecidableOne FMPZ where
  isOne = (==1)
