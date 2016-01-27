module HFlint.FMPQ.Arithmetic
where

import Control.Exception ( ArithException( RatioZeroDenominator ) )

import Data.Ratio ( numerator
                  , denominator
                  )
import Math.Structure ( NonZero(..) )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPZ
import qualified HFlint.FMPZ.Arithmetic as FMPZArith
import HFlint.FMPZ.FFI
import qualified HFlint.FMPZ.Limbs as L

import HFlint.FMPQ.FFI
import HFlint.FMPQ.Base ()

import HFlint.Internal.Lift
import HFlint.Internal.Utils ( throwBeforeIf
                             , throwBeforeIf2
                             )


{-# INLINE throwBeforeDivideByZero #-}
throwBeforeDivideByZero :: (FMPQ -> b) -> FMPQ -> b
throwBeforeDivideByZero =
  throwBeforeIf RatioZeroDenominator
  ((0/=) . liftFlint0 fmpq_is_zero)

{-# INLINE throwBeforeDivideByZero2 #-}
throwBeforeDivideByZero2 :: (a -> FMPQ -> c) -> a -> FMPQ -> c
throwBeforeDivideByZero2 =
  throwBeforeIf2 RatioZeroDenominator
  (const $ (0/=) . liftFlint0 fmpq_is_zero)


instance Enum FMPQ where
  {-# INLINE toEnum #-}
  toEnum = fromInteger . fromIntegral
  {-# INLINE fromEnum #-}
  fromEnum = fromInteger . truncate

instance Num FMPQ where
  {-# INLINE fromInteger #-}
  fromInteger a = unsafePerformIO $
    withNewFMPQ_ $ \cptr ->
    withFMPZ_ (fromInteger a) $ \aptr ->
    withNewFMPZ $ \bptr -> do
      fmpz_one bptr
      fmpq_set_fmpz_frac cptr aptr bptr

  {-# INLINE (+) #-}
  (+) = lift2Flint_ fmpq_add
  {-# INLINE (-) #-}
  (-) = lift2Flint_ fmpq_sub
  {-# INLINE (*) #-}
  (*) = lift2Flint_ fmpq_mul

  {-# INLINE negate #-}
  negate = liftFlint_ fmpq_neg
  {-# INLINE abs #-}
  abs = liftFlint_ fmpq_abs
  {-# INLINE signum #-}
  signum = fromIntegral . liftFlint0 fmpq_sgn

instance Fractional FMPQ where
  {-# INLINE fromRational #-}
  fromRational a = unsafePerformIO $
    withNewFMPQ_ $ \cptr ->
    withFMPZ (fromInteger num) $ \aptr ->
    withFMPZ (fromInteger den) $ \bptr ->
    fmpq_set_fmpz_frac cptr aptr bptr
    where
      num = numerator a
      den = denominator a

  {-# INLINE (/) #-}
  (/) = throwBeforeDivideByZero2 $
        lift2Flint_ fmpq_div
  {-# INLINE recip #-}
  recip = throwBeforeDivideByZero $
          liftFlint_ fmpq_inv

instance Real FMPQ where
  {-# INLINE toRational #-}
  toRational a = unsafePerformIO $ fmap snd $
    withFMPQ a $ \aptr -> do
      numLimbs <- L.fromCFMPZ =<< fmpq_numref aptr
      denLimbs <- L.fromCFMPZ =<< fmpq_denref aptr
      let num = toRational $ L.toInteger numLimbs
      let den = toRational $ L.toInteger denLimbs
      return (num / den)

instance RealFrac FMPQ where
  {-# INLINE properFraction #-}
  properFraction a | a == 0    = (0, 0)
                   | otherwise = (fromInteger $ fromIntegral q, r)
    where
    (q,r) = unsafePerformIO $
      withNewFMPZ  $ \qptr ->
      withNewFMPQ_ $ \bptr ->
      withFMPQ_ a  $ \aptr ->
      withNewFMPZ_ $ \rptr -> do
        numptr <- fmpq_numref aptr
        denptr <- fmpq_denref aptr
        fmpz_tdiv_qr qptr rptr numptr denptr
        fmpq_set_fmpz_frac bptr rptr denptr

{-# INLINE fromFMPZs #-}
fromFMPZs :: FMPZ -> FMPZ -> FMPQ
fromFMPZs = FMPZArith.throwBeforeDivideByZero2 $
            lift2Flint_ fmpq_set_fmpz_frac

{-# INLINE toFMPZs #-}
toFMPZs :: FMPQ -> (FMPZ,NonZero FMPZ)
toFMPZs a = unsafePerformIO $
  withNewFMPZ  $ \nptr ->
  fmap NonZero $ withNewFMPZ_ $ \dptr ->
  withFMPQ_ a  $ \aptr -> do
    numptr <- fmpq_numref aptr
    denptr <- fmpq_denref aptr
    fmpz_set nptr numptr
    fmpz_set dptr denptr
