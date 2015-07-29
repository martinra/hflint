{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , InstanceSigs
  , MultiParamTypeClasses
  , RankNTypes
  , ScopedTypeVariables
  , UndecidableInstances
  #-}

module HFlint.FMPZ.Reduction
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Proxy ( Proxy(..) )
import Data.Reflection

import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPZ.FFI

import HFlint.NMod as NMod
import HFlint.NMod.FFI


instance HasLimbHeight FMPZ where
  limbHeight a = fromIntegral $ unsafePerformIO $
    fmap snd $ withFMPZ a $ \aptr ->
    abs <$> fmpz_size aptr

instance ToNMod FMPZ where
  {-# INLINE toNMod #-}
  toNMod :: forall ctx . ReifiesNModContext ctx => FMPZ -> NMod ctx
  toNMod a = unsafePerformIO $
    fmap snd $ withFMPZ a $ \aptr -> do
      p <- nmod_n $ reflect (Proxy :: Proxy ctx)
      NMod <$> fmpz_fdiv_ui aptr p

fromNMod :: NMod ctx -> FMPZ
fromNMod = fromFlintLimb . unNMod

fromFlintLimb :: FlintLimb -> FMPZ
fromFlintLimb a = unsafePerformIO $
  withNewFMPZ_ $ \bptr ->
  fmpz_set_ui bptr a


data FMPZMod = FMPZMod FMPZ (Modulus FMPZ)

instance
     ReifiesNModContext ctx
  => ChineseRemainder FMPZMod (NMod ctx) FMPZMod
  where
  chineseRemainder (FMPZMod a (Modulus m)) (NMod a') =
    FMPZMod aa' (Modulus mm')
    where
      Modulus m' = NMod.modulus (Proxy :: Proxy ctx)
      mm' = unsafePerformIO $
        withNewFMPZ_ $ \mmptr ->
        withFMPZ m   $ \mptr  ->
        fmpz_mul_ui mmptr mptr m'
      aa' = unsafePerformIO $
        withNewFMPZ_ $ \bptr ->
        withFMPZ a   $ \aptr ->
        withFMPZ m   $ \mptr ->
        fmpz_CRT_ui bptr aptr mptr a' m' 0
