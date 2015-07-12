{-# LANGUAGE
    FlexibleContexts
  , InstanceSigs
  , ScopedTypeVariables
  #-}

module HFlint.FMPZ.Reduction
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Proxy ( Proxy(..) )
import Data.Reflection ( reflect )

import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPZ.FFI

import HFlint.NMod.Context
import HFlint.NMod.FFI
import HFlint.NMod.Reduction ( HasLimbHeight(..), ToNMod(..) )


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

{-# INLINE fromNMod #-}
fromNMod
  :: forall ctx .
     ReifiesNModContext ctx
  => NMod ctx -> FMPZ
fromNMod (NMod a) = unsafePerformIO $
  withNewFMPZ_ $ \aptr ->
  fmpz_set_ui aptr a


chineseRemainder
  :: forall ctx .
     ReifiesNModContext ctx
  => Modulus FMPZ -> FMPZ
  -> NMod ctx
  -> FMPZ
chineseRemainder (Modulus m) a (NMod a') =
 let (Modulus m') = modulusIntegral (Proxy :: Proxy ctx)
 in unsafePerformIO $
      withNewFMPZ_ $ \bptr ->
      withFMPZ a   $ \aptr ->
      withFMPZ m   $ \mptr ->
      fmpz_CRT_ui bptr aptr mptr a' m' 0
