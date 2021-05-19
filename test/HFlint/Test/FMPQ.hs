{-# LANGUAGE
    FlexibleContexts
  #-}

module HFlint.Test.FMPQ
where

import Data.Proxy
import Math.Structure.Tasty
import Test.Tasty ( testGroup , TestTree )

import HFlint.FMPQ

import HFlint.Test.FMPQ.Rational
import HFlint.FMPQ.Tasty.QuickCheck
import HFlint.FMPQ.Tasty.SmallCheck

tests :: TestTree
tests =
  testGroup "FMPQ tests" $
    [ referenceRational
    , zeroOneUnitTests
    ]
    ++
    runTestsQSC
    [ isField (Proxy :: Proxy FMPQ) ]
