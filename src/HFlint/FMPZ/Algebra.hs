{-# LANGUAGE
    MultiParamTypeClasses
  , TemplateHaskell
  #-}

module HFlint.FMPZ.Algebra
where

import Prelude ( (.), ($), (/=),  const, flip, Integer, otherwise )
import qualified Prelude as P
import Data.Maybe
import Data.Ord
import Numeric.Algebra
import Numeric.Decidable.Units
import Numeric.Decidable.Zero
import Numeric.Domain.Euclidean
import Numeric.Semiring.Integral

import HFlint.Internal.Flint
import HFlint.Internal.TH
import HFlint.FMPZ.Arithmetic ()
import HFlint.FMPZ.FFI
import HFlint.FMPZ.Internal ()


instance Additive FMPZ where
  (+) = lift2Flint_ $ const fmpz_add

instance Abelian FMPZ

instance Group FMPZ where
  (-) = lift2Flint_ $ const fmpz_sub
  negate = liftFlint_ $ const fmpz_neg
  subtract = flip (-)
  times n a = P.fromIntegral n P.* a


instance LeftModule Integer FMPZ where
  a .* b = P.fromInteger a P.* b

instance RightModule Integer FMPZ where
  a *. b = a P.* P.fromInteger b

instance LeftModule Natural FMPZ where
  a .* b = P.fromIntegral a P.* b

instance RightModule Natural FMPZ where
  a *. b = a P.* P.fromIntegral b

instance Monoidal FMPZ where
  zero = lift0FlintWithType_ FMPZType $ const fmpz_zero

instance DecidableZero FMPZ where
  isZero = (0/=) . liftFlint0 (const fmpz_is_zero)


instance Multiplicative FMPZ where
  (*) = lift2Flint_ $ const fmpz_mul

instance Commutative FMPZ

instance Unital FMPZ where
  one = lift0FlintWithType_ FMPZType $ const fmpz_one

instance DecidableUnits FMPZ where
  isUnit = (0/=) . liftFlint0 (const fmpz_is_pm1)
  recipUnit a | isUnit a  = Just a
              | otherwise = Nothing


instance Rig FMPZ where
  fromNatural = P.fromIntegral

instance Semiring FMPZ
instance IntegralSemiring FMPZ
instance Ring FMPZ where
  fromInteger = P.fromInteger


instance Euclidean FMPZ where
  splitUnit a = case P.compare a 0 of
   GT -> (1,a)
   EQ -> (1,a)
   LT -> (-1,negate a)
  degree a | isZero a  = Nothing
           | otherwise = Just $ P.fromIntegral $ P.abs a
  divide = lift2Flint2_ $ $(constTH 2) fmpz_fdiv_qr
  quot = lift2Flint_ $ const fmpz_fdiv_q
  rem = lift2Flint_ $ const fmpz_fdiv_r
  gcd a b | isZero a && isZero b = 0
          | otherwise = (lift2Flint_ $ const fmpz_gcd) a b
  xgcd a b | isZero a && isZero b = (0,1,0)
           | otherwise = (lift2Flint3_ $ $(constTH 3) fmpz_xgcd) a b
