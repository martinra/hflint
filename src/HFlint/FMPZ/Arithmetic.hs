module HFlint.FMPZ.Arithmetic
where

import Control.Exception ( ArithException( DivideByZero ) )

import qualified HFlint.FMPZ.Limbs as L

import HFlint.FMPZ.FFI

import HFlint.Internal.Lift
import HFlint.Internal.Utils ( throwBeforeIf2 )


{-# INLINE throwBeforeDivideByZero2 #-}
throwBeforeDivideByZero2 :: (a -> FMPZ -> c) -> a -> FMPZ -> c
throwBeforeDivideByZero2 =
  throwBeforeIf2 DivideByZero $
  const $ (0/=) . liftFlint0 fmpz_is_zero


instance Enum FMPZ where
  {-# INLINE toEnum #-}
  toEnum = L.toNewFMPZ . L.fromInteger . toInteger
  {-# INLINE fromEnum #-}
  fromEnum = fromEnum . L.toInteger . L.fromFMPZ

instance Num FMPZ where
  {-# INLINE fromInteger #-}
  fromInteger = L.toNewFMPZ . L.fromInteger

  {-# INLINE (+) #-}
  (+) = lift2Flint_ fmpz_add
  {-# INLINE (-) #-}
  (-) = lift2Flint_ fmpz_sub
  {-# INLINE (*) #-}
  (*) = lift2Flint_ fmpz_mul

  {-# INLINE negate #-}
  negate = liftFlint_ fmpz_neg
  {-# INLINE abs #-}
  abs = liftFlint_ fmpz_abs
  {-# INLINE signum #-}
  signum = L.toNewFMPZ . L.fromInteger . toInteger .
           liftFlint0 fmpz_sgn

instance Real FMPZ where
  {-# INLINE toRational #-}
  toRational = toRational . L.toInteger . L.fromFMPZ

instance Integral FMPZ where
  {-# INLINE quot #-}
  quot = throwBeforeDivideByZero2 $
         lift2Flint_ fmpz_tdiv_q
  {-# INLINE quotRem #-}
  quotRem = throwBeforeDivideByZero2 $
            lift2Flint2_ fmpz_tdiv_qr

  {-# INLINE div #-}
  div = throwBeforeDivideByZero2 $
        lift2Flint_ fmpz_fdiv_q
  {-# INLINE divMod #-}
  divMod = throwBeforeDivideByZero2 $
           lift2Flint2_ fmpz_fdiv_qr

  {-# INLINE toInteger #-}
  toInteger = L.toInteger . L.fromFMPZ

divexactFMPZ :: FMPZ -> FMPZ -> FMPZ
divexactFMPZ = throwBeforeDivideByZero2 $
               lift2Flint_ fmpz_divexact
