{-# LANGUAGE
    FlexibleContexts
  #-}

module FMPQTests
where

import Control.Arrow ( (***), second )
import Control.Monad ( liftM )
import Data.Composition ( (.:) )
import Data.List ( delete, intercalate )
import Data.List.Split ( splitOn )
import Data.Ratio ( (%) )

import Test.Tasty ( testGroup
                  , TestTree
                  )

import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU

import HFlint.FMPQ

import qualified TestHFlint.Utils as U
import TestHFlint.Utils ( testProperty )
import FMPQTests.Utils


fmpqTestGroup :: TestTree
fmpqTestGroup = testGroup "FMPQ Tests" [properties]

properties :: TestTree
properties = testGroup "Properties"
  [ -- Show instance
    testProperty "Show" $ equal
      (delete '(' . delete ')' . intercalate "/" . splitOn " % " . show)
      show

    -- Eq instance
  , testProperty "Eq" $ equal2 (==) (==)

    -- Ord instance
  , testProperty "Ord" $ equal2 compare compare

    -- Enum instance
  , testProperty "toEnum" $ U.equal 
      (toEnum :: Int -> FMPQ) undefined
      (fromIntegral :: Int -> Integer) truncate
  , testProperty "fromEnum" $ U.equal
      (fromInteger :: Integer -> FMPQ) undefined
       fromEnum fromEnum
    
    -- Num instance
  , testProperty "fromInteger" $ U.equal
      (fromInteger :: Integer -> FMPQ) undefined
      toRational toRational
  , testProperty "add" $ intertwining2 (+) (+)
  , testProperty "sub" $ intertwining2 (-) (-)
  , testProperty "mul" $ intertwining2 (*) (*)
  , testProperty "negate" $ intertwining negate negate
  , testProperty "abs" $ intertwining abs abs
  , testProperty "signum" $ intertwining signum signum

    -- Fractional instance
  , testProperty "toRational . fromRational" $ intertwining id id
  , testProperty "division (/)" $ U.intertwining2
      (liftM fromRational :: Maybe Rational -> Maybe FMPQ) (liftM toRational)
      (U.wrapDivideByZero2 (/)) (U.wrapDivideByZero2 (/))
  , testProperty "recip" $ U.intertwining
      (liftM fromRational :: Maybe Rational -> Maybe FMPQ) (liftM toRational)
      (U.wrapDivideByZero recip) (U.wrapDivideByZero recip)

    -- RealFrac instance
  , testProperty "properFraction" $ equal
      properFraction (second toRational . properFraction)

    -- various functions
  , testProperty "fromFMPZs" $ U.equal2
      (id :: Maybe Integer -> Maybe Integer) undefined
      (U.wrapDivideByZero2 $
       curry $ uncurry fromFMPZs . (fromInteger***fromInteger))
      (U.wrapDivideByZero2 $ fromRational .: (%))
  ] 
