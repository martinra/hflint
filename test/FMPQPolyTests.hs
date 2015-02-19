{-# LANGUAGE
    FlexibleContexts
  #-}

module FMPQPolyTests
where

import Control.Arrow ( second )
import Data.List ( delete
                 , intercalate )
import Data.List.Split ( splitOn )

import Test.Tasty ( testGroup
                  , TestTree
                  )

import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU

import HFlint.FMPQ
import HFlint.FMPQPoly

import qualified TestHFlint.Utils as U


fmpqPolyTestGroup :: TestTree
fmpqPolyTestGroup = testGroup "FMPQ Tests" []

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
    testProperty "Eq" $ equal2 (==) (==)
  ] 
