{-# LANGUAGE
    FlexibleContexts
  #-}

module FMPQTests
where

import Control.Arrow ( second
                     , (***)
                     )
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
import qualified TestHFlint.Utils as U


fmpqTestGroup :: TestTree
fmpqTestGroup = testGroup "FMPQ Tests" [properties]


-- We need to specify the type, so that a is not specialized when infering the
-- type on first occurence of equal and equal2
equal :: Eq a => (Rational -> a) -> (FMPQ -> a) -> Rational -> Bool
equal2 :: Eq a => (Rational -> Rational -> a) -> (FMPQ -> FMPQ -> a) -> Rational -> Rational -> Bool
equal         = U.equal (fromRational :: Rational -> FMPQ) toRational
equal2        = U.equal2 (fromRational :: Rational -> FMPQ) toRational
intertwining  = U.intertwining (fromRational :: Rational -> FMPQ) toRational
intertwining2 = U.intertwining2 (fromRational :: Rational -> FMPQ) toRational

preRecip :: (Eq a, Num a) => a -> a
preRecip a | a == 0    = 1
           | otherwise = a


testProperty s p = testGroup ("s " ++ "(QuickCheck & SmallCheck)")
  [ QC.testProperty s p,
    SC.testProperty s p
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ -- Show instance
    testProperty "Show" $ equal (delete '(' . delete ')' .
                                 intercalate "/" . splitOn " % " . show)
                                show

    -- Eq instance
  , testProperty "Eq" $ equal2 (==) (==)

    -- Ord instance
  , testProperty "Ord" $ equal2 compare compare

    -- Enum instance
  , testProperty "toEnum" $
      U.equal (toEnum :: Int -> FMPQ) undefined
              (fromIntegral :: Int -> Integer) truncate
  , testProperty "fromEnum" $
      U.equal (fromInteger :: Integer -> FMPQ) undefined fromEnum fromEnum
    
    -- Num instance
  , testProperty "fromInteger" $ 
      U.equal (fromInteger :: Integer -> FMPQ) undefined toRational toRational
  , testProperty "add" $ intertwining2 (+) (+)
  , testProperty "sub" $ intertwining2 (-) (-)
  , testProperty "mul" $ intertwining2 (*) (*)
  , testProperty "negate" $ intertwining negate negate
  , testProperty "abs" $ intertwining abs abs
  , testProperty "signum" $ intertwining signum signum

    -- Fractional instance
  , testProperty "fromRational" $ intertwining id id
  , testProperty "div" $ intertwining2 (\x y -> x/preRecip y)
                                       (\x y -> x/preRecip y)
  , testProperty "recip" $ intertwining (recip . preRecip)
                                        (recip . preRecip)

    -- RealFrac instance
  , testProperty "properFraction" $
      equal properFraction ((id *** toRational) . properFraction)
  ] 
