{-# LANGUAGE
    FlexibleContexts
  #-}

module HFlint.Test.FMPQPoly
where

import Data.Function ( on )
import Math.Structure.Tasty
import Test.Tasty ( testGroup , TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import HFlint.FMPQPoly
import HFlint.FMPZPoly


tests :: TestTree
tests = testGroup "FMPQPoly tests"
  [ unitTests
  , properties
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ -- Eq instance
    testPropertyQSnC 2 "Eq" $
      intertwiningInnerPairing fromRationals
      ((==) `on` reverse . dropWhile (0==) . reverse)
      (==)

    -- conversion from and to FMPZPoly
  , testPropertyQSC "fromFMPZPoly" $
      intertwiningMorphisms (id :: [Integer] -> [Integer])
      (fromRationals . map fromInteger)
      (fromFMPZPoly . fromIntegers)

--  , runTestsQSC "toFMPZPoly" $ I.equal
--      fromRationals undefined
--      id
--      (uncurry (.*) . ((flip fromFMPZs 1)***fromFMPZPoly) . toFMPZPoly)

    -- factorization
--  , runTestsQSC "factor" $ I.equal
--      fromRationals undefined
--      id
--      (unfactor . factor)
  ] 


unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ -- Show instance
    testCase "Show Deg -1" $ 
      show (fromRationals []) @?= "0"
  , testCase "Show Deg 0" $ 
      show (fromRationals [1/2]) @?= "1/2"
  , testCase "Show Deg 1" $
      show (fromRationals [1/2,1/4]) @?= "1/4*T+1/2"
  , testCase "Show Deg 2" $ 
      show (fromRationals [2/7,1/4,3/5]) @?= "3/5*T^2 + 1/4*T + 2/7"
  ]
