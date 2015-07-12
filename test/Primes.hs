{-# LANGUAGE
    FlexibleContexts
  #-}

module Primes
where

import Test.Tasty ( testGroup, TestTree )

import Primes.UnitTests ( primesUnitTests )


primesTestGroup :: TestTree
primesTestGroup =
  testGroup "Primes Tests"
  [ primesUnitTests
  ]
