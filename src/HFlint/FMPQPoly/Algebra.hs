{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving
  #-}

module HFlint.FMPQPoly.Algebra
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
-- import qualified Prelude as P

import Control.Arrow ( first )
import qualified Data.Vector as V
import Math.Structure

import HFlint.FMPQ
import HFlint.FMPQ.FFI ()
import HFlint.FMPQPoly.Base
import HFlint.FMPQPoly.FFI
import HFlint.Internal.Lift


instance AdditiveMagma FMPQPoly where
  (+) = lift2Flint_ fmpq_poly_add

instance Abelian FMPQPoly

instance AdditiveSemigroup FMPQPoly

instance AdditiveMonoid FMPQPoly where
  zero = lift0Flint_ fmpq_poly_zero

instance DecidableZero FMPQPoly where
  isZero = (/=0) . (liftFlint0 fmpq_poly_is_zero)

instance AdditiveGroup FMPQPoly where
  negate = liftFlint_ fmpq_poly_neg
  (-) = lift2Flint_ fmpq_poly_sub


instance MultiplicativeMagma FMPQPoly where
  (*) = lift2Flint_ fmpq_poly_mul

instance Commutative FMPQPoly

instance MultiplicativeSemigroup FMPQPoly

instance MultiplicativeMonoid FMPQPoly where
  one = lift0Flint_ fmpq_poly_one

instance DecidableOne FMPQPoly where
  isOne = (/=0) . (liftFlint0 fmpq_poly_is_one)


deriving instance MultiplicativeMagma (NonZero FMPQPoly)

instance Commutative (NonZero FMPQPoly)

instance MultiplicativeSemigroup (NonZero FMPQPoly)

deriving instance MultiplicativeMonoid (NonZero FMPQPoly)

deriving instance DecidableOne (NonZero FMPQPoly)


instance Distributive FMPQPoly

instance Semiring FMPQPoly

instance Rng FMPQPoly

instance Rig FMPQPoly

instance Ring FMPQPoly

instance IntegralDomain FMPQPoly

instance FactorialRing FMPQPoly where
  factor a = Factored (fromList [fromFMPZs one d] * fromFMPZPoly u)
                      (V.map (first fromFMPZPoly) f)
    where
    (d, a') = toFMPZPoly a
    Factored u f = factor a' 

instance PIDomain FMPQPoly where
  gcd = lift2Flint_ fmpq_poly_gcd
  xgcd = lift2Flint3_ fmpq_poly_xgcd

instance EuclideanDomain FMPQPoly where
  quotRem = lift2Flint2_ fmpq_poly_divrem
  quot = lift2Flint_ fmpq_poly_div
  rem = lift2Flint_ fmpq_poly_rem
  euclNorm a | isZero a = Nothing
             | otherwise = Just $ fromIntegral $
                           (liftFlint0 fmpq_poly_degree) a
