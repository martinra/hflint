
{-# LANGUAGE
    FlexibleContexts
  #-}

module FMPZTests.Algebra
where

import Control.Arrow ( first, second, (***) )
import Control.Monad ( liftM )
import Data.Composition hiding ( (.*) )
import Data.Maybe
import qualified Prelude as P
import Prelude ( ($), (.), id, undefined
               , fmap
               , curry, uncurry
               , Integer, (==)
               )
import Numeric.Algebra
import Numeric.Domain.Euclidean
import Numeric.Decidable.Zero
import Numeric.Decidable.Units
import Test.Tasty
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ( (@=?) )

import HFlint.FMPZ

import qualified TestHFlint.Utils as U
import TestHFlint.Utils ( testProperty )
import FMPZTests.Utils


algebraProperties :: TestTree
algebraProperties = testGroup "FMPZ Algebra"
  [ testProperty "(+)" $ intertwining2 (+) (+)

  , testProperty "(-)" $ intertwining2 (-) (-)
  , testProperty "negate" $ intertwining negate negate
  , testProperty "negate" $ intertwining2 subtract subtract
--  , testProperty "(.*)" $ U.equal
--      (id***P.fromInteger :: (Natural,Integer) -> (Natural,FMPZ)) undefined
--      (uncurry times) (P.toInteger . uncurry times)

  , testProperty "(.*)" $ U.equal
      (second P.fromInteger :: (Integer,Integer) -> (Integer,FMPZ)) undefined
      (uncurry (.*)) (P.toInteger . uncurry (.*))
  , testProperty "(*.)" $ U.equal
      (first P.fromInteger :: (Integer,Integer) -> (FMPZ,Integer)) undefined
      (uncurry (*.)) (P.toInteger . uncurry (*.))

--  , testProperty "(.*)" $ U.equal
--      (second P.fromInteger :: (Natural,Integer) -> (Natural,FMPZ)) undefined
--      (uncurry (.*)) (P.toInteger . uncurry (.*))
--  , testProperty "(*.)" $ U.equal
--      (first P.fromInteger :: (Integer,Natural) -> (FMPZ,Natural)) undefined
--      (uncurry (*.)) (P.toInteger . uncurry (*.))

  , testProperty "isZero" $ equal
      isZero isZero

  , testProperty "isUnit" $ equal
      isUnit isUnit
  , testProperty "recipUnit" $ equal
      recipUnit (fmap P.toInteger . recipUnit)

--  , testProperty "fromNatural" $ U.equal
--      (Natural -> Natural) undefined
--      fromNatural (P.toInteger . fromNatural)

  , testProperty "fromIntegral" $ U.equal
      (id :: Integer -> Integer) undefined
      id (P.toInteger . (fromInteger :: Integer -> FMPZ))

  , testProperty "splitUnit" $ equal
      splitUnit ((P.toInteger***P.toInteger) . splitUnit)
  , testProperty "degree" $ equal degree degree
  , testProperty "divide" $ U.equal2
      (liftM fromInteger :: Maybe Integer -> Maybe FMPZ) undefined
      (U.wrapDivideByZero2 divide)
      (liftM (P.toInteger***P.toInteger) .: U.wrapDivideByZero2 divide)
  , testProperty "quot" $ U.equal2
      (liftM fromInteger :: Maybe Integer -> Maybe FMPZ) undefined
      (U.wrapDivideByZero2 quot)
      (liftM P.toInteger .: U.wrapDivideByZero2 quot)
  , testProperty "rem" $ U.equal2
      (liftM fromInteger :: Maybe Integer -> Maybe FMPZ) undefined
      (U.wrapDivideByZero2 rem)
      (liftM P.toInteger .: U.wrapDivideByZero2 rem)
  , testProperty "gcd" $ equal2 
      gcd (P.toInteger .: gcd)
  , testProperty "xgcd" $
     \x y -> let x' = fromInteger (x :: Integer) :: FMPZ
                 y' = fromInteger (y :: Integer) :: FMPZ
                 (g,s,t) = xgcd x' y'
             in g == s*x' + t*y'
  ]

algebraUnitTests :: TestTree
algebraUnitTests = testGroup "Algebra Unit Tests"
  [ HU.testCase "zero" $ 
      (zero :: Integer) @=? P.toInteger (zero :: FMPZ)
  , HU.testCase "one" $ 
      (one :: Integer) @=? P.toInteger (one :: FMPZ)
  ]

