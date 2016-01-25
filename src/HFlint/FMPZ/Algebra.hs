{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving
  , TemplateHaskell
  #-}

module HFlint.FMPZ.Algebra
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Math.Structure.Additive
import Math.Structure.Multiplicative ()
import Math.Structure.Instances.TH.Additive
import Math.Structure.Instances.TH.Multiplicative
import Math.Structure.Instances.TH.Ring

import HFlint.FMPZ.Arithmetic ()
import HFlint.FMPZ.FFI
import HFlint.Internal.Lift


mkAbelianGroupInstanceFromNum (return []) [t|FMPZ|]
mkCommutativeMonoidInstanceFromNum (return []) [t|FMPZ|]
mkCommutativeMonoidInstanceFromNonZeroNum (return []) [t|FMPZ|]
mkEuclideanDomainInstanceFromIntegralWithCustomGCD (return []) [t|FMPZ|]
  [| lift2Flint_ fmpz_gcd |]
  [| lift2Flint3_ fmpz_xgcd |]
  [| lift2Flint_ fmpz_lcm |]
