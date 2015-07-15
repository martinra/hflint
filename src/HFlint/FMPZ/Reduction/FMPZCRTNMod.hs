{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , InstanceSigs
  , MultiParamTypeClasses
  , RankNTypes
  , ScopedTypeVariables
  #-}

module HFlint.FMPZ.Reduction.FMPZCRTNMod
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


newtype FMPZCRTNMod ctx = FMPZCRTNMod { unFMPZCRTNMod :: FMPZ }
data FMPZCRTNModData = FMPZCRTNModData
  { fmpzModulus :: FMPZ
  , preinvertNModModulus :: FlintLimb
  , modinvFMPZModulus :: FlintLimb
  }
type ReifiesFMPZCRTNModContext ctx ctx' = Reifies ctx' FMPZCRTNModData

withFMPZCRTNModContext
  :: forall ctx b
  .  ReifiesNModContext ctx
  => Modulus FMPZ -> Proxy ctx
  -> (forall ctx' . ReifiesFMPZCRTNModContext ctx ctx' => Proxy ctx' -> b)
  -> b
withFMPZCRTNModContext (Modulus m) proxyCtx = reify fmpzCRTNModData
  where
    Modulus m' = NMod.modulus proxyCtx
    fmpzCRTNModData = FMPZCRTNModData
      { fmpzModulus = m
      , preinvertNModModulus = unsafePerformIO $ n_preinvert_limb m'
      , modinvFMPZModulus = unsafePerformIO $
          fmap snd $ withFMPZ m $ \mptr -> do
            c <- fmpz_fdiv_ui mptr m'
            nmod_inv c $ reflect proxyCtx
      }


instance
     ( ReifiesNModContext ctx, ReifiesFMPZCRTNModContext ctx ctx' )
  => ChineseRemainder FMPZ (NMod ctx) (FMPZCRTNMod ctx')
  where
    chineseRemainder a (NMod a') = FMPZCRTNMod $ unsafePerformIO $
      withNewFMPZ_ $ \bptr ->
      withFMPZ a   $ \aptr ->
      withFMPZ m   $ \mptr ->
      fmpz_CRT_ui_precomp bptr aptr mptr a' m' m'preinv nullPtr minv 0
      where
        fmpzCRTNModData = reflect (Proxy :: Proxy ctx')
        m = fmpzModulus fmpzCRTNModData
        (Modulus m') = NMod.modulus (Proxy :: Proxy ctx)
        m'preinv = preinvertNModModulus fmpzCRTNModData
        minv = modinvFMPZModulus fmpzCRTNModData
