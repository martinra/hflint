module HFlint.FMPZ.Tasty.QuickCheck
where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import HFlint.FMPZ.Arithmetic ()
import HFlint.FMPZ.FFI


instance Arbitrary FMPZ where
  arbitrary = fmap fromInteger (arbitrary :: Gen Integer)
  shrink = shrinkIntegral
