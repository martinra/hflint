{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , UndecidableInstances
  #-}

module HFlint.NMod.Tasty.QuickCheck
where

import Data.Maybe ( mapMaybe )
import Math.Structure ( Unit(..), DecidableUnit(..) )
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import HFlint.NMod.Algebra ()
import HFlint.NMod.Arithmetic ()
import HFlint.NMod.Context
import HFlint.NMod.FFI


instance ReifiesNModContext ctx => Arbitrary (NMod ctx) where
  arbitrary = fmap fromIntegral (arbitrary :: Gen Int)
  shrink = const []

instance ReifiesNModContext ctx => Arbitrary (Unit (NMod ctx)) where
  arbitrary = toUnit <$> arbitrary `suchThat` isUnit
  shrink = mapMaybe toUnitSafe . shrink . fromUnit
