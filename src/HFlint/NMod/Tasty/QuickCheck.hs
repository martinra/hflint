{-# LANGUAGE
    FlexibleContexts
  , UndecidableInstances
  #-}

module HFlint.NMod.Tasty.QuickCheck
where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import HFlint.NMod.Arithmetic ()
import HFlint.NMod.Context
import HFlint.NMod.FFI


instance ReifiesNModContext ctx => Arbitrary (NMod ctx) where
  arbitrary = fmap fromIntegral (arbitrary :: Gen Int)
  shrink = const []
