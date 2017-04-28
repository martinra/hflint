{-# LANGUAGE
    FlexibleInstances
  #-}

module HFlint.FMPZ.Tasty.QuickCheck
where

import Math.Structure ( Unit(..), isUnit )
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import HFlint.FMPZ.Algebra ()
import HFlint.FMPZ.Arithmetic ()
import HFlint.FMPZ.FFI


instance Arbitrary FMPZ where
  arbitrary = fmap fromInteger (arbitrary :: Gen Integer)
  shrink = shrinkIntegral

instance Arbitrary (Unit FMPZ) where
  arbitrary = Unit <$> arbitrary `suchThat` isUnit
  shrink = map Unit . filter isUnit . shrink . fromUnit
