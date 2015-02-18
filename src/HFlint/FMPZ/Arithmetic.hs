module HFlint.FMPZ.Arithmetic
where

import Control.Exception ( ArithException( DivideByZero ) )

import qualified HFlint.FMPZ.Limbs as L

import HFlint.FMPZ.FFI
import HFlint.FMPZ.Internal ()

import HFlint.Internal.Flint
import HFlint.Internal.Utils ( throwBeforeIf2 )


throwBeforeDivideByZero2 :: (a -> FMPZ -> c) -> a -> FMPZ -> c
throwBeforeDivideByZero2 =
  throwBeforeIf2 DivideByZero
  (const $ (1==) . liftFlint0 (const fmpz_is_zero))


instance Enum FMPZ where
  toEnum = L.toNewFMPZ . L.fromInteger . toInteger
  fromEnum = fromEnum . L.toInteger . L.fromFMPZ

instance Num FMPZ where
    fromInteger = L.toNewFMPZ . L.fromInteger

    (+) = lift2Flint_ $ const fmpz_add
    (-) = lift2Flint_ $ const fmpz_sub
    (*) = lift2Flint_ $ const fmpz_mul

    negate = liftFlint_ $ const fmpz_neg
    abs = liftFlint_ $ const fmpz_abs
    signum = L.toNewFMPZ . L.fromInteger . toInteger .
             liftFlint0 (const fmpz_sgn)

instance Real FMPZ where
  toRational = toRational . L.toInteger . L.fromFMPZ

instance Integral FMPZ where
  -- todo: use specialized methods
  quot = throwBeforeDivideByZero2
         (lift2Flint_ $ const fmpz_tdiv_q)
  quotRem = throwBeforeDivideByZero2 $
            lift2Flint2_ $ const $ const fmpz_tdiv_qr

  div = throwBeforeDivideByZero2 $
        lift2Flint_ $ const fmpz_fdiv_q
  divMod = throwBeforeDivideByZero2 $
           lift2Flint2_ $ const $ const fmpz_fdiv_qr

  toInteger = L.toInteger . L.fromFMPZ

