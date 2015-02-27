module HFlint.FMPQ.Arithmetic
where

import Control.Exception ( ArithException( RatioZeroDenominator ) )

import Data.Ratio ( numerator
                  , denominator
                  )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPZ
import qualified HFlint.FMPZ.Arithmetic as FMPZArith
import HFlint.FMPZ.FFI
import qualified HFlint.FMPZ.Limbs as L

import HFlint.FMPQ.FFI
import HFlint.FMPQ.Internal
import HFlint.FMPQ.Base ()

import HFlint.Internal.Flint
import HFlint.Internal.Utils ( throwBeforeIf
                             , throwBeforeIf2
                             )


throwBeforeDivideByZero :: (FMPQ -> b) -> FMPQ -> b
throwBeforeDivideByZero =
  throwBeforeIf RatioZeroDenominator
  ((0/=) . liftFlint0 (const fmpq_is_zero))

throwBeforeDivideByZero2 :: (a -> FMPQ -> c) -> a -> FMPQ -> c
throwBeforeDivideByZero2 =
  throwBeforeIf2 RatioZeroDenominator
  (const $ (0/=) . liftFlint0 (const fmpq_is_zero))


instance Enum FMPQ where
  toEnum = fromInteger . fromIntegral
  fromEnum = fromInteger . truncate

instance Num FMPQ where
  fromInteger a = unsafePerformIO $
                  withNewFMPQ_ $ \_ cptr ->
                  withFMPZ_ (fromInteger a) $ \_ aptr ->
                  withNewFMPZ $ \_ bptr -> do
                    fmpz_one bptr
                    fmpq_set_fmpz_frac cptr aptr bptr

  (+) = lift2Flint_ $ const fmpq_add
  (-) = lift2Flint_ $ const fmpq_sub
  (*) = lift2Flint_ $ const fmpq_mul

  negate = liftFlint_ $ const fmpq_neg
  abs = liftFlint_ $ const fmpq_abs
  signum = fromIntegral . liftFlint0 (const fmpq_sgn)

instance Fractional FMPQ where
  fromRational a = unsafePerformIO $
                   withNewFMPQ_ $ \_ cptr ->
                   withFMPZ (fromInteger num) $ \_ aptr ->
                   withFMPZ (fromInteger den) $ \_ bptr ->
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
                 withFMPQ a $ \_ aptr -> do
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
      withNewFMPZ  $ \_ qptr ->
      withNewFMPQ_ $ \_ bptr ->
      withFMPQ_ a  $ \_ aptr ->
      withNewFMPZ_ $ \_ rptr -> do
        numptr <- fmpq_numref aptr
        denptr <- fmpq_denref aptr
        fmpz_tdiv_qr qptr rptr numptr denptr
        fmpq_set_fmpz_frac bptr rptr denptr

fromFMPZs :: FMPZ -> FMPZ -> FMPQ
fromFMPZs = FMPZArith.throwBeforeDivideByZero2 $
            lift2FlintWithType_ FMPQType
            (const fmpq_set_fmpz_frac)
