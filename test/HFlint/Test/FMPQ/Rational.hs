module HFlint.Test.FMPQ.Rational
where

import Control.Arrow ( (***), second )
import Data.Composition ( (.:) )
import Data.List ( delete, intercalate )
import Data.List.Split ( splitOn )
import Data.Ratio ( (%) )
import qualified Math.Structure as M
import qualified Math.Structure.Tasty as MT

import Test.Tasty ( testGroup , TestTree )
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit as HU ( (@?=), (@=?) )

import HFlint.FMPQ
import HFlint.Test.Utility.FMPQ
import qualified HFlint.Test.Utility.Intertwine as I


referenceRational :: TestTree
referenceRational = testGroup "Properties"
  [ -- Show instance
    MT.testPropertyQSC "Show" $ equal
      (delete '(' . delete ')' . intercalate "/" . splitOn " % " . show)
      show

    -- Eq instance
  , MT.testPropertyQSC "Eq" $ equal2 (==) (==)

    -- Ord instance
  , MT.testPropertyQSC "Ord" $ equal2 compare compare

    -- Enum instance
  , MT.testPropertyQSC "toEnum" $ I.equal 
      (toEnum :: Int -> FMPQ) undefined
      (fromIntegral :: Int -> Integer) truncate
  , MT.testPropertyQSC "fromEnum" $ I.equal
      (fromInteger :: Integer -> FMPQ) undefined
       fromEnum fromEnum
    
    -- Num instance
  , MT.testPropertyQSC "fromInteger" $ I.equal
      (fromInteger :: Integer -> FMPQ) undefined
      toRational toRational
  , MT.testPropertyQSC "add" $ intertwining2 (+) (+)
  , MT.testPropertyQSC "sub" $ intertwining2 (-) (-)
  , MT.testPropertyQSC "mul" $ intertwining2 (*) (*)
  , MT.testPropertyQSC "negate" $ intertwining negate negate
  , MT.testPropertyQSC "abs" $ intertwining abs abs
  , MT.testPropertyQSC "signum" $ intertwining signum signum

    -- Fractional instance
  , MT.testPropertyQSC "toRational . fromRational" $ intertwining id id
  , MT.testPropertyQSC "division (/)" $ I.intertwining2
      (fmap fromRational :: Maybe Rational -> Maybe FMPQ) (fmap toRational)
      (I.wrapDivideByZero2 (/)) (I.wrapDivideByZero2 (/))
  , MT.testPropertyQSC "recip" $ I.intertwining
      (fmap fromRational :: Maybe Rational -> Maybe FMPQ) (fmap toRational)
      (I.wrapDivideByZero recip) (I.wrapDivideByZero recip)

    -- RealFrac instance
  , MT.testPropertyQSC "properFraction" $ equal
      properFraction (second toRational . properFraction)

    -- various functions
  , MT.testPropertyQSC "fromFMPZs" $ I.equal2
      (id :: Maybe Integer -> Maybe Integer) undefined
      (I.wrapDivideByZero2 $
       curry $ uncurry fromFMPZs . (fromInteger***fromInteger))
      (I.wrapDivideByZero2 $ fromRational .: (%))
  ] 

zeroOneUnitTests :: TestTree
zeroOneUnitTests = testGroup "Zero & One Unit Tests"
  [ HU.testCase "zero" $ 
      fromInteger (M.zero :: Integer) @=? (M.zero :: FMPQ)
  , HU.testCase "one" $ 
      fromInteger (M.one :: Integer) @=? (M.one :: FMPQ)
  ]
