{-# LANGUAGE
    FlexibleInstances
  #-}

module HFlint.FMPQ.Tasty.QuickCheck
where

import Data.Maybe ( mapMaybe )
import Math.Structure ( Unit(..), DecidableUnit(..) )
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import HFlint.FMPQ.Algebra ()
import HFlint.FMPQ.Arithmetic ()
import HFlint.FMPQ.FFI


instance Arbitrary FMPQ where
  arbitrary = fmap fromRational (arbitrary :: Gen Rational)
  shrink = shrinkRealFrac

instance Arbitrary (Unit FMPQ) where
  arbitrary = Unit <$> arbitrary `suchThat` isUnit
  shrink = mapMaybe toUnitSafe . shrink . fromUnit
