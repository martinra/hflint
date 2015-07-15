{-# LANGUAGE
    FlexibleContexts
  , GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  , StandaloneDeriving
  #-}

module HFlint.NMod.Reduction
where

import Data.Maybe
import Data.Word ( Word64 )

import HFlint.NMod.Context 
import HFlint.NMod.FFI


-- number of limbs used to recover a number is a logarithmic height
class HasLimbHeight a where
  limbHeight :: a -> Word64


class ToNMod a where
  toNMod :: ReifiesNModContext ctx => a -> NMod ctx

class ToNModMay a where
  toNModMay :: ReifiesNModContext ctx => a -> Maybe (NMod ctx)


deriving instance HasLimbHeight a => HasLimbHeight (Modulus a)
