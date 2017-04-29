module HFlint.Test.FMPQ.Rational
where

import Control.Arrow ( (***), second )
import Data.Composition ( (.:) )
import Data.List ( delete, intercalate )
import Data.List.Split ( splitOn )
import Data.Ratio ( (%) )
import Math.Structure.Tasty
import Test.Tasty ( testGroup , TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), (@=?) )
import qualified Math.Structure as M

import HFlint.FMPQ
import HFlint.Test.Utility.DivisionByZero


referenceRational :: TestTree
referenceRational = testGroup "Properties"
  [ -- Show instance
    testPropertyQSC "Show" $
      intertwiningMorphisms (fromRational :: Rational -> FMPQ)
      (delete '(' . delete ')' . intercalate "/" . splitOn " % " . show)
      show

    -- Eq instance
  , testPropertyQSC "Eq" $
      intertwiningInnerPairing (fromRational :: Rational -> FMPQ)
      (==) (==)

    -- Ord instance
  , testPropertyQSC "Ord" $
      intertwiningInnerPairing (fromRational :: Rational -> FMPQ)
      compare compare

    -- Enum instance
  , testPropertyQSC "toEnum" $
      intertwiningMorphisms (toEnum :: Int -> FMPQ)
      (fromIntegral :: Int -> Integer) truncate

  , testPropertyQSC "fromEnum" $
      intertwiningMorphisms (fromInteger :: Integer -> FMPQ)
      fromEnum fromEnum
    
    -- Num instance
  , testPropertyQSC "fromInteger" $
      intertwiningMorphisms (fromInteger :: Integer -> FMPQ)
      toRational toRational
  , testPropertyQSC "add" $
      intertwiningBinaryOperators (fromRational :: Rational -> FMPQ)
      (+) (+)
  , testPropertyQSC "sub" $
      intertwiningBinaryOperators (fromRational :: Rational -> FMPQ)
      (-) (-)
  , testPropertyQSC "mul" $
      intertwiningBinaryOperators (fromRational :: Rational -> FMPQ)
      (*) (*)
  , testPropertyQSC "negate" $
      intertwiningEndomorphisms (fromRational :: Rational -> FMPQ)
      negate negate
  , testPropertyQSC "abs" $
      intertwiningEndomorphisms (fromRational :: Rational -> FMPQ)
      abs abs
  , testPropertyQSC "signum" $
      intertwiningEndomorphisms (fromRational :: Rational -> FMPQ)
      signum signum

    -- Fractional instance
  , testPropertyQSC "toRational . fromRational" $
      intertwiningMorphisms (fromRational :: Rational -> FMPQ)
      id toRational
  , testPropertyQSC "division (/)" $
      intertwiningBinaryOperators (fmap fromRational :: Maybe Rational -> Maybe FMPQ)
      (wrapDivideByZero2 (/)) (wrapDivideByZero2 (/))
  , testPropertyQSC "recip" $
      intertwiningEndomorphisms (fmap fromRational :: Maybe Rational -> Maybe FMPQ)
      (wrapDivideByZero recip) (wrapDivideByZero recip)

    -- RealFrac instance
  , testPropertyQSC "properFraction" $
      intertwiningMorphisms (fromRational :: Rational -> FMPQ)
      properFraction (second toRational . properFraction)

    -- various functions
  , testPropertyQSC "fromFMPZs" $
      intertwiningInnerPairing (id :: Maybe Integer -> Maybe Integer)
      (wrapDivideByZero2 $
         curry $ uncurry fromFMPZs . (fromInteger***fromInteger))
      (wrapDivideByZero2 $ fromRational .: (%))
  ] 

zeroOneUnitTests :: TestTree
zeroOneUnitTests = testGroup "Zero & One Unit Tests"
  [ testCase "zero" $ 
      fromInteger (M.zero :: Integer) @=? (M.zero :: FMPQ)
  , testCase "one" $ 
      fromInteger (M.one :: Integer) @=? (M.one :: FMPQ)
  ]
