{-# LANGUAGE
    FlexibleContexts
  #-}

module FMPQTests
where

import Data.Proxy
import Math.Structure.Tasty
import Test.Tasty ( testGroup , TestTree )

import HFlint.FMPQ
import FMPQTests.Rational

import TestHFlint.Utils ( testProperty )


fmpqTestGroup :: TestTree
fmpqTestGroup =
  testGroup "FMPQ Tests" $
  [ referenceRational
  , zeroOneUnitTests
  ]
  ++
  ( (`runTestR` testProperty) $
      concat <$> sequence
      [ isField (Proxy :: Proxy FMPQ) ]
  )
