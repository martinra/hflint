module HFlint.FMPQ.Tasty.QuickCheck
where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import HFlint.FMPQ.Arithmetic ()
import HFlint.FMPQ.FFI


instance Arbitrary FMPQ where
  arbitrary = fmap fromRational (arbitrary :: Gen Rational)
  shrink = shrinkRealFrac
