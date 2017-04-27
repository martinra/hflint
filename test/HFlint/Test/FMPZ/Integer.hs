module HFlint.Test.FMPZ.Integer
where

import Control.Arrow ( (***) )
import Data.Composition ( (.:) )
import qualified Math.Structure as M
import qualified Math.Structure.Tasty as MT

import Test.Tasty ( testGroup, TestTree )
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ( (@?=), (@=?) )

import HFlint.FMPZ
import HFlint.Test.Utility.FMPZ
import qualified HFlint.Test.Utility.Intertwine as I


-- Integer is a reference implementation for FMPZ
referenceIngeger :: TestTree
referenceIngeger = testGroup "Compare with Integer"
  [ -- Show instance
    MT.testPropertyQSC "Show" $ equal show show 

    -- Eq instance
  , MT.testPropertyQSC "Eq" $ equal2 (==) (==)

    -- Ord instance
  , MT.testPropertyQSC "Ord" $ equal2 compare compare

    -- Enum instance
  , MT.testPropertyQSC "toEnum" $
      I.equal (toEnum :: Int -> FMPZ) undefined
              (fromIntegral :: Int -> Integer) toInteger
  , MT.testPropertyQSC "fromEnum" $
      I.equal (fromInteger :: Integer -> FMPZ) undefined fromEnum fromEnum

    -- Num instance
  , MT.testPropertyQSC "toInteger . fromInteger" $ intertwining id id
  , MT.testPropertyQSC "add" $ intertwining2 (+) (+)
  , MT.testPropertyQSC "sub" $ intertwining2 (-) (-)
  , MT.testPropertyQSC "mul" $ intertwining2 (*) (*)
  , MT.testPropertyQSC "negate" $ intertwining negate negate
  , MT.testPropertyQSC "abs" $ intertwining abs abs 
  , MT.testPropertyQSC "signum" $ intertwining signum signum

    -- Real instace

    -- Integral instance
  , MT.testPropertyQSC "quot" $ I.intertwining2
      (fmap fromInteger :: Maybe Integer -> Maybe FMPZ) (fmap toInteger)
      (I.wrapDivideByZero2 quot) (I.wrapDivideByZero2 quot)
  , MT.testPropertyQSC "quotRem" $ I.equal2
      (fmap fromInteger :: Maybe Integer -> Maybe FMPZ) undefined
      (I.wrapDivideByZero2 quotRem)
      (fmap (toInteger *** toInteger) .: I.wrapDivideByZero2 quotRem)
  , MT.testPropertyQSC "div" $ I.intertwining2
      (fmap fromInteger :: Maybe Integer -> Maybe FMPZ) (fmap toInteger)
      (I.wrapDivideByZero2 div) (I.wrapDivideByZero2 div)
  , MT.testPropertyQSC "divMod" $ I.equal2
      (fmap fromInteger :: Maybe Integer -> Maybe FMPZ) undefined
      (I.wrapDivideByZero2 divMod)
      (fmap (toInteger *** toInteger) .: I.wrapDivideByZero2 divMod)
  ]

zeroOneUnitTests :: TestTree
zeroOneUnitTests = testGroup "Zero & One Unit Tests"
  [ HU.testCase "zero" $ 
      fromInteger (M.zero :: Integer) @=? (M.zero :: FMPZ)
  , HU.testCase "one" $ 
      fromInteger (M.one :: Integer) @=? (M.one :: FMPZ)
  ]
