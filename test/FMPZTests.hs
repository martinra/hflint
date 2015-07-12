{-# LANGUAGE
    FlexibleContexts
  #-}

module FMPZTests
where

import Data.Proxy
import Math.Structure.Tasty
import Test.Tasty ( testGroup, TestTree )

import HFlint.FMPZ
import FMPZTests.Integer

import TestHFlint.Utils ( testProperty )


fmpzTestGroup :: TestTree
fmpzTestGroup =
  testGroup "FMPZ Tests" $
  [ referenceIngeger
  , zeroOneUnitTests
  ]
  ++
  ( (`runTestR` testProperty) $
      concat <$> sequence
      [ isEuclideanDomain (Proxy :: Proxy FMPZ) ]
  )
