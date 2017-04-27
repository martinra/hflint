{-# LANGUAGE
    FlexibleContexts
  #-}

module HFlint.Test.Primes
where

import Test.Tasty ( testGroup, TestTree )

import HFlint.Test.Primes.UnitTests ( unitTests )


tests :: TestTree
tests =
  testGroup "Primes tests"
  [ unitTests
  ]
