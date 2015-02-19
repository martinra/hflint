{-# LANGUAGE
    FlexibleContexts
  #-}

module FMPQPolyTests
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

import HFlint.FMPQ
import HFlint.FMPQPoly

import qualified TestHFlint.Utils as U


fmpqPolyTestGroup :: TestTree
fmpqPolyTestGroup = testGroup "FMPQPoly Tests" [ properties, unitTests ]

-- We need to specify the type, so that a is not specialized when infering the
-- type on first occurence of equal and equal2
equal :: Eq a => ([Rational] -> a) -> (FMPQPoly -> a) -> [Rational] -> Bool
equal2 :: Eq a => ([Rational] -> [Rational] -> a) -> (FMPQPoly -> FMPQPoly -> a) -> [Rational] -> [Rational] -> Bool
equal         = U.equal (fromRationals :: [Rational] -> FMPQPoly) toRationals
equal2        = U.equal2 (fromRationals :: [Rational] -> FMPQPoly) toRationals
intertwining  = U.intertwining (fromRationals :: [Rational] -> FMPQPoly) toRationals
intertwining2 = U.intertwining2 (fromRationals :: [Rational] -> FMPQPoly) toRationals


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
      show (fromRationals []) @?= "0"
  , HU.testCase "Show Deg 0" $ 
      show (fromRationals [1/2]) @?= "1/2"
  , HU.testCase "Show Deg 1" $ show (fromRationals [1/2,1/4]) @?= "1/4*T+1/2" , HU.testCase "Show Deg 2" $ 
      show (fromRationals [2/7,1/4,3/5]) @?= "3/5*T^2 + 1/4*T + 2/7"
  ]
