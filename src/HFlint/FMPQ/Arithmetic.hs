module HFlint.FMPQ.Arithmetic
where

import Control.Exception ( ArithException( DivideByZero ) )

import Data.Ratio ( numerator
                  , denominator
                  )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPZ
import HFlint.FMPZ.FFI
import qualified HFlint.FMPZ.Limbs as L

import HFlint.FMPQ.FFI
import HFlint.FMPQ.Internal
import HFlint.FMPQ.Basic ()

import HFlint.Internal.Flint
import HFlint.Internal.Utils ( throwBeforeIf
                             , throwBeforeIf2
                             )


throwBeforeDivideByZero :: (FMPQ -> b) -> FMPQ -> b
throwBeforeDivideByZero =
  throwBeforeIf DivideByZero
  ((1==) . liftFlint0 (const fmpq_is_zero))

throwBeforeDivideByZero2 :: (a -> FMPQ -> c) -> a -> FMPQ -> c
throwBeforeDivideByZero2 =
  throwBeforeIf2 DivideByZero
  (const $ (1==) . liftFlint0 (const fmpq_is_zero))


instance Enum FMPQ where
  toEnum = fromInteger . fromIntegral
  fromEnum = fromInteger . truncate

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
  signum = fromInteger . toInteger .
           liftFlint0 (const fmpq_sgn)

instance Fractional FMPQ where
  fromRational a = unsafePerformIO $
                   withNewFMPQ_ $ const $ \cptr ->
                   withFMPZ (fromInteger num) $ const $ \aptr ->
                   withFMPZ (fromInteger den) $ const $ \bptr ->
                   fmpq_set_fmpz_frac cptr aptr bptr
    where
    num = numerator a
    den = denominator a

  (/) = throwBeforeDivideByZero2 $
        lift2Flint_ (const fmpq_div)
  recip = throwBeforeDivideByZero $
          liftFlint_ (const fmpq_inv)

instance Real FMPQ where
  toRational a = unsafePerformIO $ fmap snd $
                 withFMPQ a $ const $ \aptr -> do
                   numLimbs <- L.fromCFMPZ =<< fmpq_numref aptr
                   denLimbs <- L.fromCFMPZ =<< fmpq_denref aptr
                   let num = toRational $ L.toInteger numLimbs
                   let den = toRational $ L.toInteger denLimbs
                   return (num / den)

instance RealFrac FMPQ where
  properFraction a | a == 0    = (0, 0)
                   | otherwise = (fromInteger $ fromIntegral q, r)
    where
    (q,r) = unsafePerformIO $
      withNewFMPZ $ const $ \qptr ->
      withNewFMPQ_ $ const $ \bptr ->
      withFMPQ_ a $ const $ \aptr ->
      withNewFMPZ_ $ const $ \rptr -> do
        numptr <- fmpq_numref aptr
        denptr <- fmpq_denref aptr
        fmpz_tdiv_qr qptr rptr numptr denptr
        fmpq_set_fmpz_frac bptr rptr denptr
