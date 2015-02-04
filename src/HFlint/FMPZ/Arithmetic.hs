module HFlint.FMPZ.Arithmetic
where

import qualified HFlint.FMPZ.Limbs as L

import HFlint.FMPZ.FFI
import HFlint.FMPZ.Internal ()
import HFlint.Internal.Flint


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
             (liftFlint0 $ const fmpz_sgn)

instance Real FMPZ where
  toRational = toRational . L.toInteger . L.fromFMPZ

instance Integral FMPZ where
  -- todo: use specialized methods
  quot = lift2Flint_ $ const fmpz_tdiv_q
  quotRem = lift2Flint2_ $ const $ const fmpz_tdiv_qr

  div = lift2Flint_ $ const fmpz_fdiv_q
  divMod = lift2Flint2_ $ const $ const fmpz_tdiv_qr

  toInteger = L.toInteger . L.fromFMPZ
