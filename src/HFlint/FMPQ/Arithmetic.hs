module Flint.FMPQ.Arithmetic
where

import Flint.Internal.Flint

import Flint.FMPZ
import Flint.FMPZ.FFI

import Flint.FMPQ.FFI
import Flint.FMPQ.Internal
import Flint.FMPQ.Basic

import Data.Ratio (numerator, denominator)

import System.IO.Unsafe (unsafePerformIO)


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
