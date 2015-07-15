{-# LANGUAGE
    FlexibleContexts
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , StandaloneDeriving
  #-}

module HFlint.NMod.Reduction
where

import Data.Proxy
import Data.Reflection
import Data.Word ( Word64 )
import Math.Structure ( MultiplicativeMagma, MultiplicativeSemigroup
                      , Commutative )

import System.IO.Unsafe ( unsafePerformIO )

import HFlint.NMod.Context 
import HFlint.NMod.FFI


-- number of limbs used to recover a number is a logarithmic height
class HasLimbHeight a where
  limbHeight :: a -> Word64


class ToNMod a where
  toNMod :: ReifiesNModContext ctx => a -> NMod ctx

class ToNModMay a where
  toNModMay :: ReifiesNModContext ctx => a -> Maybe (NMod ctx)


newtype Modulus a = Modulus a
  deriving ( Eq, Show )

deriving instance HasLimbHeight a => HasLimbHeight (Modulus a)

deriving instance MultiplicativeMagma a => MultiplicativeMagma (Modulus a)
deriving instance MultiplicativeSemigroup a => MultiplicativeSemigroup (Modulus a)
deriving instance Commutative a => Commutative (Modulus a)



{-# INLINE modulus #-}
modulus :: forall ctx . ReifiesNModContext ctx => Proxy ctx -> Modulus FlintLimb
modulus = modulusIntegral

{-# INLINE modulusIntegral #-}
modulusIntegral
  :: forall ctx a .
     ( ReifiesNModContext ctx, Integral a )
  => Proxy ctx -> Modulus a
modulusIntegral proxy = Modulus $ fromIntegral $ unsafePerformIO $ nmod_n (reflect proxy)


class ChineseRemainder a b c where
  chineseRemainder :: a -> b -> c
