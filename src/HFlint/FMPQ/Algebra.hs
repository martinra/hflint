{-# LANGUAGE
    TemplateHaskell
  #-}

module HFlint.FMPQ.Algebra
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

import HFlint.FMPQ.Arithmetic ()
import HFlint.FMPQ.FFI

mkAbeleanGroupInstanceFromNum ''FMPQ
mkCommutativeGroupInstanceFromFractional ''FMPQ
mkFieldInstance ''FMPQ
