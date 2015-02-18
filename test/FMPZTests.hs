{-# LANGUAGE
    FlexibleContexts
  #-}

module FMPZTests
where

import Control.Applicative ( (<$>) )
import Control.Arrow ( (***) )
import Control.Exception ( handle
                         , ArithException( DivideByZero )
                         , evaluate
                         )
import Control.Monad ( liftM
                     , liftM2
                     )
import Data.Composition ( (.:) )
import Test.Tasty ( testGroup,
                    TestTree
                  )
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPZ
import qualified TestHFlint.Utils as U


fmpzTestGroup :: TestTree
fmpzTestGroup = testGroup "FMPZ Tests" [properties]


-- We need to specify the type, so that a is not specialized when infering the
-- type on first occurence of equal and equal2
equal :: Eq a => (Integer -> a) -> (FMPZ -> a) -> Integer -> Bool
equal2 :: Eq a => (Integer -> Integer -> a) -> (FMPZ -> FMPZ -> a) -> Integer -> Integer -> Bool
equal         = U.equal (fromInteger :: Integer -> FMPZ) toInteger
equal2        = U.equal2 (fromInteger :: Integer -> FMPZ) toInteger
intertwining  = U.intertwining (fromInteger :: Integer -> FMPZ) toInteger
intertwining2 = U.intertwining2 (fromInteger :: Integer -> FMPZ) toInteger


testProperty s p = testGroup "(QuickCheck & SmallCheck)"
  [ QC.testProperty s p,
    SC.testProperty s p
  ]

properties :: TestTree
properties = testGroup "Properties"
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
      (wrapDivideByZero quot) (wrapDivideByZero quot)
  , testProperty "quotRem" $ U.equal2
      (liftM fromInteger :: Maybe Integer -> Maybe FMPZ) undefined
      (wrapDivideByZero quotRem)
      (liftM (toInteger *** toInteger) .: wrapDivideByZero quotRem)
  , testProperty "div" $ U.intertwining2
      (liftM fromInteger :: Maybe Integer -> Maybe FMPZ) (liftM toInteger)
      (wrapDivideByZero div) (wrapDivideByZero div)
  , testProperty "divMod" $ U.equal2
      (liftM fromInteger :: Maybe Integer -> Maybe FMPZ) undefined
      (wrapDivideByZero divMod)
      (liftM (toInteger *** toInteger) .: wrapDivideByZero divMod)
  ]

wrapDivideByZero :: (a -> b -> c)
                 -> Maybe a -> Maybe b -> Maybe c
wrapDivideByZero f a b = unsafePerformIO $
  handle (\DivideByZero -> return Nothing) $
  case liftM2 f a b of
    Nothing -> return Nothing
    Just c' -> Just <$> evaluate c'
