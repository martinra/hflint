{-# LANGUAGE
    FlexibleContexts
  #-}

module HFlint.Test.FMPZPoly
where

import Data.Function ( on )
import Math.Structure.Tasty
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import HFlint.FMPZPoly


tests :: TestTree
tests = testGroup "FMPZPoly tests"
  [ unitTests
  , properties
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ -- Eq instance
    -- we use only QuickCheck, because SmallCheck is yields too slow tests
    testPropertyQC "Eq" $
      intertwiningInnerPairing fromIntegers
      ((==) `on` reverse . dropWhile (0==) . reverse)
      (==)
  ] 

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ -- Show instance
    testCase "Show Deg -1" $ 
      show (fromIntegers []) @?= "0"
  , testCase "Show Deg 0" $ 
      show (fromIntegers [1]) @?= "1"
  , testCase "Show Deg 1" $ 
      show (fromIntegers [2,4]) @?= "4*T+2"
  , testCase "Show Deg 2" $ 
      show (fromIntegers [7,4,15]) @?= "15*T^2+4*T+7"
  ]
