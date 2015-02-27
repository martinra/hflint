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


fmpzTestGroup :: TestTree
fmpzTestGroup = testGroup "FMPZ Tests"
                [ referenceIngeger
                , zeroOneUnitTests
                , testGroup "Euclidean Domain" $
                    isEuclideanDomain (Proxy :: Proxy FMPZ)
                ]
