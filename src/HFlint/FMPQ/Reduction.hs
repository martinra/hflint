{-# LANGUAGE
    FlexibleContexts
  , InstanceSigs
  , ScopedTypeVariables
  #-}

module HFlint.FMPQ.Reduction
where

import Data.Proxy ( Proxy(..) )
import Data.Reflection ( reflect )

import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPQ.FFI
import HFlint.FMPZ.FFI

import HFlint.NMod.Context
import HFlint.NMod.FFI
import HFlint.NMod.Reduction


instance HasLimbHeight FMPQ where
  limbHeight a = fromIntegral $ unsafePerformIO $
    fmap snd $ withFMPQ a $ \aptr -> do
      numptr <- fmpq_numref aptr
      denptr <- fmpq_denref aptr
      numht <- abs <$> fmpz_size numptr
      denht <- abs <$> fmpz_size denptr
      return $ 2 * max numht denht


instance ToNModMay FMPQ where
  {-# INLINE toNModMay #-}
  toNModMay :: forall ctx . ReifiesNModContext ctx => FMPQ -> Maybe (NMod ctx)
  toNModMay a = unsafePerformIO $
    fmap snd $ withFMPQ a $ \aptr -> do
      p <- nmod_n $ reflect (Proxy :: Proxy ctx)
      numptr <- fmpq_numref aptr
      denptr <- fmpq_denref aptr
      num <- fmpz_fdiv_ui numptr p
      den <- fmpz_fdiv_ui denptr p
      if den == 0
        then return Nothing
        else Just <$> NMod <$>
               nmod_div num den (reflect (Proxy :: Proxy ctx))


data RationalReconstructionType =
    Balanced
  | Bounded FMPZ FMPZ

{-# INLINE rationalReconstruct #-}
rationalReconstruct
  :: RationalReconstructionType
  -> Modulus FMPZ -> FMPZ -> Maybe FMPQ
rationalReconstruct Balanced (Modulus m) a =
  let (b,r) = unsafePerformIO $
        withNewFMPQ            $ \bptr ->
        fmap snd $ withFMPZ m  $ \mptr ->
        fmap snd $ withFMPZ a  $ \aptr ->
        fmpq_reconstruct_fmpz bptr aptr mptr
  in if r == 0 then Nothing else Just b
rationalReconstruct (Bounded num den) (Modulus m) a =
  let (b,r) = unsafePerformIO $
        withNewFMPQ             $ \bptr   ->
        fmap snd $ withFMPZ num $ \numptr ->
        fmap snd $ withFMPZ den $ \denptr ->
        fmap snd $ withFMPZ m   $ \mptr   ->
        fmap snd $ withFMPZ a   $ \aptr   ->
        fmpq_reconstruct_fmpz_2 bptr aptr mptr numptr denptr
  in if r == 0 then Nothing else Just b
