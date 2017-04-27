{-# LANGUAGE
    FlexibleContexts
  #-}

module HFlint.Test.FMPZPoly
where

import Data.Function ( on )
import qualified Data.Vector as V
import Math.Structure.Tasty
import Test.Tasty ( testGroup, TestTree )

import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ( (@?=) )

import HFlint.FMPZPoly

import HFlint.Test.Utility.FMPZPoly


tests :: TestTree
tests = testGroup "FMPZPoly tests"
  [ unitTests
  , properties
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ -- Eq instance
    -- we use only QuickCheck, because SmallCheck is yields too slow tests
    testPropertyQC "Eq" $ equal2
    ((==) `on` V.toList . V.reverse . V.dropWhile (0==) . V.reverse . V.fromList) (==)
  ] 

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ -- Show instance
    HU.testCase "Show Deg -1" $ 
      show (fromIntegers []) @?= "0"
  , HU.testCase "Show Deg 0" $ 
      show (fromIntegers [1]) @?= "1"
  , HU.testCase "Show Deg 1" $ 
      show (fromIntegers [2,4]) @?= "4*T+2"
  , HU.testCase "Show Deg 2" $ 
      show (fromIntegers [7,4,15]) @?= "15*T^2+4*T+7"
  ]
