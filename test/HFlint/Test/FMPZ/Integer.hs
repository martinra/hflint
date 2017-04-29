module HFlint.Test.FMPZ.Integer
where

import Control.Arrow ( (***) )
import Data.Composition ( (.:) )
import Math.Structure.Tasty
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), (@=?) )
import qualified Math.Structure as M

import HFlint.FMPZ
import HFlint.Test.Utility.DivisionByZero


-- Integer is a reference implementation for FMPZ
referenceIngeger :: TestTree
referenceIngeger = testGroup "Compare with Integer"
  [ -- Show instance
    testPropertyQSC "Show" $
      intertwiningMorphisms (fromInteger :: Integer -> FMPZ)
      show show 

    -- Eq instance
  , testPropertyQSC "Eq" $
      intertwiningInnerPairing (fromInteger :: Integer -> FMPZ)
      (==) (==)

    -- Ord instance
  , testPropertyQSC "Ord" $
      intertwiningInnerPairing (fromInteger :: Integer -> FMPZ)
      compare compare

    -- Enum instance
  , testPropertyQSC "toEnum" $
      intertwiningMorphisms (toEnum :: Int -> FMPZ)
      (fromIntegral :: Int -> Integer) toInteger

  , testPropertyQSC "fromEnum" $
      intertwiningMorphisms (fromInteger :: Integer -> FMPZ)
      fromEnum fromEnum

    -- Num instance
  , testPropertyQSC "toInteger . fromInteger" $
      intertwiningMorphisms (fromInteger :: Integer -> FMPZ)
      id toInteger
  , testPropertyQSC "add" $ 
      intertwiningBinaryOperators (fromInteger :: Integer -> FMPZ)
      (+) (+)
  , testPropertyQSC "sub" $
      intertwiningBinaryOperators (fromInteger :: Integer -> FMPZ)
      (-) (-)
  , testPropertyQSC "mul" $
      intertwiningBinaryOperators (fromInteger :: Integer -> FMPZ)
      (*) (*)
  , testPropertyQSC "negate" $
      intertwiningEndomorphisms (fromInteger :: Integer -> FMPZ)
      negate negate
  , testPropertyQSC "abs" $
      intertwiningEndomorphisms (fromInteger :: Integer -> FMPZ)
      abs abs
  , testPropertyQSC "signum" $
      intertwiningEndomorphisms (fromInteger :: Integer -> FMPZ)
      signum signum

    -- Real instance

    -- Integral instance
  , testPropertyQSC "quot" $
      intertwiningBinaryOperators (fmap fromInteger :: Maybe Integer -> Maybe FMPZ)
      (wrapDivideByZero2 quot) (wrapDivideByZero2 quot)
  , testPropertyQSC "quotRem" $
      intertwiningInnerPairing (fmap fromInteger :: Maybe Integer -> Maybe FMPZ)
      (fmap (fromInteger *** fromInteger) .: wrapDivideByZero2 quotRem)
      (wrapDivideByZero2 quotRem)
  , testPropertyQSC "div" $
      intertwiningBinaryOperators (fmap fromInteger :: Maybe Integer -> Maybe FMPZ)
      (wrapDivideByZero2 div) (wrapDivideByZero2 div)
  , testPropertyQSC "divMod" $
      intertwiningInnerPairing (fmap fromInteger :: Maybe Integer -> Maybe FMPZ)
      (fmap (fromInteger *** fromInteger) .: wrapDivideByZero2 divMod)
      (wrapDivideByZero2 divMod)
  ]

zeroOneUnitTests :: TestTree
zeroOneUnitTests = testGroup "Zero & One Unit Tests"
  [ testCase "zero" $ 
      fromInteger (M.zero :: Integer) @=? (M.zero :: FMPZ)
  , testCase "one" $ 
      fromInteger (M.one :: Integer) @=? (M.one :: FMPZ)
  ]
