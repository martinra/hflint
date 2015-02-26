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
import qualified Prelude as P

import Math.Structure.Instances.TH.Additive
import Math.Structure.Instances.TH.Multiplicative
import Math.Structure.Instances.TH.Ring

import HFlint.FMPZ.Arithmetic ()
import HFlint.FMPZ.FFI

mkAbeleanGroupInstanceFromNum ''FMPZ
mkCommutativeMonoidInstanceFromNum ''FMPZ
mkEuclideanDomainInstanceFromIntegral ''FMPZ
