{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  , TemplateHaskell
  , TypeSynonymInstances
  #-}

module HFlint.NMod.Algebra
where


import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
-- import qualified Prelude as P


import Control.Monad.Reader
import Control.Applicative ( liftA )
import Math.Structure.Additive
import Math.Structure.Instances.TH.Additive
import Math.Structure.Instances.TH.Multiplicative
import Math.Structure.Instances.TH.Ring
import Math.Structure.Multiplicative

import HFlint.Internal.Context
import HFlint.NMod.Arithmetic ()
import HFlint.NMod.FFI


mkAbelianGroupInstanceFromNum ''RNMod
mkCommutativeGroupInstanceFromNonZeroFractional ''RNMod
mkFieldInstance ''RNMod

instance DecidableZeroM (RFlint NModCtx) NMod where
  isZeroM = liftA $ (==0) . unNMod

instance DecidableOneM (RFlint NModCtx) NMod where
  isOneM = liftA $ (==1) . unNMod
