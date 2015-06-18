module HFlint.FMPZ.Arithmetic
where

import Control.Exception ( ArithException( DivideByZero ) )

import qualified HFlint.FMPZ.Limbs as L

import HFlint.FMPZ.FFI

import HFlint.Internal.Lift
import HFlint.Internal.Utils ( throwBeforeIf2 )


throwBeforeDivideByZero2 :: (a -> FMPZ -> c) -> a -> FMPZ -> c
throwBeforeDivideByZero2 =
  throwBeforeIf2 DivideByZero $
  const $ (0/=) . liftFlint0 fmpz_is_zero


instance Enum FMPZ where
  toEnum = L.toNewFMPZ . L.fromInteger . toInteger
  fromEnum = fromEnum . L.toInteger . L.fromFMPZ

instance Num FMPZ where
    fromInteger = L.toNewFMPZ . L.fromInteger

    (+) = lift2Flint_ fmpz_add
    (-) = lift2Flint_ fmpz_sub
    (*) = lift2Flint_ fmpz_mul

    negate = liftFlint_ fmpz_neg
    abs = liftFlint_ fmpz_abs
    signum = L.toNewFMPZ . L.fromInteger . toInteger .
             liftFlint0 fmpz_sgn

instance Real FMPZ where
  toRational = toRational . L.toInteger . L.fromFMPZ

instance Integral FMPZ where
  quot = throwBeforeDivideByZero2 $
         lift2Flint_ fmpz_tdiv_q
  quotRem = throwBeforeDivideByZero2 $
            lift2Flint2_ fmpz_tdiv_qr

  div = throwBeforeDivideByZero2 $
        lift2Flint_ fmpz_fdiv_q
  divMod = throwBeforeDivideByZero2 $
           lift2Flint2_ fmpz_fdiv_qr

  toInteger = L.toInteger . L.fromFMPZ

