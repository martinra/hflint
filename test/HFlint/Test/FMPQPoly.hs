{-# LANGUAGE
    FlexibleContexts
  #-}

module HFlint.Test.FMPQPoly
where

import Data.Function ( on )
import Math.Structure.Tasty
import qualified Data.Vector as V

import Test.Tasty ( testGroup
                  , TestTree
                  )

import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ( (@?=) )

import HFlint.FMPQPoly
import HFlint.FMPZPoly

import HFlint.Test.Utility.FMPQPoly
import qualified HFlint.Test.Utility.Intertwine as I


tests :: TestTree
tests = testGroup "FMPQPoly tests"
  [ unitTests
  , properties
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ -- Eq instance
    testPropertyQSnC 2 "Eq" $ equal2
    ( let
      dropTrailingZeros = V.toList . V.reverse . V.dropWhile (0==) .
                          V.reverse . V.fromList
      in (==) `on` dropTrailingZeros )
    (==)

    -- conversion from and to FMPZPoly
  , testPropertyQSC "fromFMPZPoly" $ I.equal
      (id :: [Integer] -> [Integer]) undefined
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
    HU.testCase "Show Deg -1" $ 
      show (fromRationals []) @?= "0"
  , HU.testCase "Show Deg 0" $ 
      show (fromRationals [1/2]) @?= "1/2"
  , HU.testCase "Show Deg 1" $ show (fromRationals [1/2,1/4]) @?= "1/4*T+1/2" , HU.testCase "Show Deg 2" $ 
      show (fromRationals [2/7,1/4,3/5]) @?= "3/5*T^2 + 1/4*T + 2/7"
  ]
