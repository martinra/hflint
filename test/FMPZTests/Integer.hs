module FMPZTests.Integer
where

import Control.Arrow ( (***) )
import Control.Monad ( liftM )
import Data.Composition ( (.:) )
import qualified Math.Structure as M

import Test.Tasty ( testGroup, TestTree )
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit as HU ( (@?=), (@=?) )

import HFlint.FMPZ
import qualified TestHFlint.Utils as U
import TestHFlint.Utils ( testProperty )
import FMPZTests.Utils


-- Integer is a reference implementation for FMPZ
referenceIngeger :: TestTree
referenceIngeger = testGroup "Compare with Integer"
  [ -- Show instance
    testProperty "Show" $ equal show show 

    -- Eq instance
  , testProperty "Eq" $ equal2 (==) (==)

    -- Ord instance
  , testProperty "Ord" $ equal2 compare compare

    -- Enum instance
  , testProperty "toEnum" $
      U.equal (toEnum :: Int -> FMPZ) undefined
              (fromIntegral :: Int -> Integer) toInteger
  , testProperty "fromEnum" $
      U.equal (fromInteger :: Integer -> FMPZ) undefined fromEnum fromEnum

    -- Num instance
  , testProperty "toInteger . fromInteger" $ intertwining id id
  , testProperty "add" $ intertwining2 (+) (+)
  , testProperty "sub" $ intertwining2 (-) (-)
  , testProperty "mul" $ intertwining2 (*) (*)
  , testProperty "negate" $ intertwining negate negate
  , testProperty "abs" $ intertwining abs abs 
  , testProperty "signum" $ intertwining signum signum

    -- Real instace

    -- Integral instance
  , testProperty "quot" $ U.intertwining2
      (liftM fromInteger :: Maybe Integer -> Maybe FMPZ) (liftM toInteger)
      (U.wrapDivideByZero2 quot) (U.wrapDivideByZero2 quot)
  , testProperty "quotRem" $ U.equal2
      (liftM fromInteger :: Maybe Integer -> Maybe FMPZ) undefined
      (U.wrapDivideByZero2 quotRem)
      (liftM (toInteger *** toInteger) .: U.wrapDivideByZero2 quotRem)
  , testProperty "div" $ U.intertwining2
      (liftM fromInteger :: Maybe Integer -> Maybe FMPZ) (liftM toInteger)
      (U.wrapDivideByZero2 div) (U.wrapDivideByZero2 div)
  , testProperty "divMod" $ U.equal2
      (liftM fromInteger :: Maybe Integer -> Maybe FMPZ) undefined
      (U.wrapDivideByZero2 divMod)
      (liftM (toInteger *** toInteger) .: U.wrapDivideByZero2 divMod)
  ]

zeroOneUnitTests :: TestTree
zeroOneUnitTests = testGroup "Zero & One Unit Tests"
  [ HU.testCase "zero" $ 
      fromInteger (M.zero :: Integer) @=? (M.zero :: FMPZ)
  , HU.testCase "one" $ 
      fromInteger (M.one :: Integer) @=? (M.one :: FMPZ)
  ]
