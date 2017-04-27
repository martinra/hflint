{-# LANGUAGE
    FlexibleInstances
  #-}

module HFlint.FMPQ.Tasty.QuickCheck
where

import Math.Structure ( Unit(..), isUnit )
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
  shrink = map Unit . filter isUnit . shrink . fromUnit
