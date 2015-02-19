{-# LANGUAGE
    FlexibleContexts
  #-}

module FMPZPolyTests
where

import Control.Arrow ( (***) )
import Data.Composition ( (.:) )
import Data.Function ( on )
import Data.List ( delete
                 , intercalate
                 , dropWhileEnd
                 )
import Data.List.Split ( splitOn )
import qualified Data.Vector as V

import Test.Tasty ( testGroup
                  , TestTree
                  )

import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ( (@?=) )

import HFlint.FMPZ
import HFlint.FMPZPoly

import qualified TestHFlint.Utils as U


fmpzPolyTestGroup :: TestTree
fmpzPolyTestGroup = testGroup "FMPZPoly Tests" [ properties, unitTests ]

-- We need to specify the type, so that a is not specialized when infering the
-- type on first occurence of equal and equal2
equal :: Eq a => ([Integer] -> a) -> (FMPZPoly -> a) -> [Integer] -> Bool
equal2 :: Eq a => ([Integer] -> [Integer] -> a) -> (FMPZPoly -> FMPZPoly -> a) -> [Integer] -> [Integer] -> Bool
equal         = U.equal (fromIntegers :: [Integer] -> FMPZPoly) toIntegers
equal2        = U.equal2 (fromIntegers :: [Integer] -> FMPZPoly) toIntegers
intertwining  = U.intertwining (fromIntegers :: [Integer] -> FMPZPoly) toIntegers
intertwining2 = U.intertwining2 (fromIntegers :: [Integer] -> FMPZPoly) toIntegers


testProperty s p = testGroup ("(QuickCheck & SmallCheck)")
  [ QC.testProperty s p
  , SC.testProperty s p
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ -- Eq instance
    -- we use only QuickCheck, because SmallCheck is yields too slow tests
    QC.testProperty "Eq" $ equal2
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
