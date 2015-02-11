module Flint.FMPQ.Arithmetic
where

import Data.Ratio ( numerator
                  , denominator
                  )

import Flint.Internal.Flint

import Flint.FMPZ
import Flint.FMPZ.FFI

import Flint.FMPQ.FFI
import Flint.FMPQ.Internal
import Flint.FMPQ.Basic


instance Enum FMPQ where
  fromEnum = fromInteger . fromEnum
  toEnum = toEnum . truncate

instance Num FMPQ where
  fromInteger a = unsafePerformIO $
                  withNewFMPQ_ $ const $ \cptr ->
                  withFMPZ_ (fromInteger a) $ const $ \aptr ->
                  withNewFMPZ $ const $ \bptr -> do
                    fmpz_one bptr
                    fmpq_set_fmpz_frac cptr aptr bptr

  (+) = lift2Flint_ $ const fmpq_add
  (-) = lift2Flint_ $ const fmpq_sub
  (*) = lift2Flint_ $ const fmpq_mul

  negate = liftFlint_ $ const fmpq_neg
  abs = liftFlint_ $ const fmpq_abs
  signum = fromInteger . toInteger
           (lift0Flint $ const fmpq_sgn)

instance RealFrac FMPQ where
-- CONITINUE
  properFraction a | a == 0    = (fromInteger 0 :: FMPZ, fromInteger 0)
                   | otherwise = unsafePerformIO $
                                 withNewFMPQ_ $ const $ \cptr ->
                                 withNewFMPZ_ $ const $ \dptr ->
                                 
 (q, fromInteger r / n)
                                 where
                                 n = num

instance Fractional FMPQ where
  fromRational a = unsafePerformIO $
                   withNewFMPQ_ $ const $ \cptr ->
                   withFMPZ (fromInteger num) $ const $ \aptr ->
                   withFMPZ (fromInteger den) $ const $ \bptr ->
                   fmpq_set_fmpz_frac cptr aptr bptr
    where
    num = numerator a
    den = denominator a

  (/) = lift2Flint_ $ const fmpq_div
  recip = liftFlint_ $ const fmpq_inv

instance Real FMPQ where
  toRational a = unsafePerformIO $ withFMPZ a $ const $ \aptr ->
                 let num = L.toInteger $ L.fromCFMPZ $ fmpq_numref a
                     den = L.toInteger $ L.fromCFMPZ $ fmpq_denref a
                 in return (num / den)

instance Num FMPQ where
    fromInteger a = unsafePerformIO $
                    withNewFMPQ_ $ const $ \cptr ->
                    withFMPZ_ (fromInteger a) $ const $ \aptr ->
                    withNewFMPZ $ const $ \bptr -> do
                      fmpz_one bptr
                      fmpq_set_fmpz_frac cptr aptr bptr
    (+) = lift2Flint_ $ const fmpq_add
    (-) = lift2Flint_ $ const fmpq_sub
    (*) = lift2Flint_ $ const fmpq_mul
    abs = liftFlint_ $ const fmpq_abs
    signum = fromInteger . fromIntegral . unsafePerformIO .
             lift0Flint (const fmpq_sgn)
    negate = liftFlint_ $ const fmpq_neg


instance Fractional FMPQ where
    fromRational a = unsafePerformIO $
                     withNewFMPQ_ $ const $ \cptr ->
                     withFMPZ (fromInteger num) $ const $ \aptr ->
                     withFMPZ (fromInteger den) $ const $ \bptr ->
                     fmpq_set_fmpz_frac cptr aptr bptr
        where
          num = numerator a
          den = denominator a

    (/) = lift2Flint_ $ const fmpq_div
    recip = liftFlint_ $ const fmpq_inv
