{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , InstanceSigs
  , MultiParamTypeClasses
  , RankNTypes
  , ScopedTypeVariables
  #-}

module HFlint.FMPZ.Reduction.FMPZCRTFlintLimb
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Proxy ( Proxy(..) )
import Data.Reflection
import Foreign.Ptr ( nullPtr ) 

import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPZ.FFI

import HFlint.NMod as NMod
import HFlint.NMod.FFI


newtype FMPZCRTFlintLimb ctx = FMPZCRTFlintLimb { unFMPZCRTFlintLimb :: FMPZ }
data FMPZCRTFlintLimbData = FMPZCRTFlintLimbData
  { fmpzModulus :: FMPZ
  , modinvFMPZModulus :: FlintLimb
  , flintLimbModulus :: FlintLimb
  , preinvertFlintLimbModulus :: FlintLimb
  }
type ReifiesFMPZCRTFlintLimbContext ctx = Reifies ctx FMPZCRTFlintLimbData

withFMPZCRTFlintLimbContext
  :: Modulus FMPZ -> Modulus FlintLimb
  -> (forall ctx . ReifiesFMPZCRTFlintLimbContext ctx => Proxy ctx -> b)
  -> b
withFMPZCRTFlintLimbContext (Modulus m) (Modulus m') = reify fmpzCRTFlintLimbData
  where
    fmpzCRTFlintLimbData = FMPZCRTFlintLimbData
      { fmpzModulus = m
      , modinvFMPZModulus =
          withNModContext m' $ \nmocCtx ->
          unsafePerformIO $
          fmap snd $ withFMPZ m $ \mptr -> do
            c <- fmpz_fdiv_ui mptr m'
            nmod_inv c $ reflect nmocCtx
      , flintLimbModulus = m'
      , preinvertFlintLimbModulus = unsafePerformIO $ n_preinvert_limb m'
      }


instance
     ReifiesFMPZCRTFlintLimbContext ctx
  => ChineseRemainder FMPZ FlintLimb (FMPZCRTFlintLimb ctx)
  where
    chineseRemainder a a' = FMPZCRTFlintLimb $ unsafePerformIO $
      withNewFMPZ_ $ \bptr ->
      withFMPZ a   $ \aptr ->
      withFMPZ m   $ \mptr ->
      fmpz_CRT_ui_precomp bptr aptr mptr a' m' m'preinv nullPtr minv 0
      where
        fmpzCRTFlintLimbData = reflect (Proxy :: Proxy ctx)

        m = fmpzModulus fmpzCRTFlintLimbData
        minv = modinvFMPZModulus fmpzCRTFlintLimbData
        m' = flintLimbModulus fmpzCRTFlintLimbData
        m'preinv = preinvertFlintLimbModulus fmpzCRTFlintLimbData
