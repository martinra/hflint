{-# LANGUAGE
    FlexibleContexts
  #-}

module FMPQTests
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


fmpqTestGroup :: TestTree
fmpqTestGroup = testGroup "FMPQ Tests" [properties]


equal :: Eq a
      => (Rational -> a)
      -> (FMPQ -> a)
      -> Rational
      -> Bool
equal f g x = f x == g (fromRational x)

equal2 :: Eq a
       => (Rational -> Rational -> a)
       -> (FMPQ -> FMPQ -> a)
       -> Rational -> Rational
       -> Bool
equal2 f g x y = f x y == g (fromRational x) (fromRational y)

intertwining :: (Rational -> Rational)
             -> (FMPQ -> FMPQ)
             -> Rational
             -> Bool
intertwining f g x =
  f x == toRational ( g (fromRational x))

intertwining2 :: (Rational -> Rational -> Rational)
              -> (FMPQ -> FMPQ -> FMPQ)
              -> Rational -> Rational
              -> Bool
intertwining2 f g x y =
  f x y == toRational ( g (fromRational x) (fromRational y))

preRecip :: (Eq a, Num a)
         => a -> a
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
       \x -> (truncate (toEnum x :: FMPQ) :: Integer) == toEnum (x :: Int)
   , testProperty "fromEnum" $
       \x -> fromEnum (fromInteger x :: FMPQ) == fromEnum (x :: Integer)
     
     -- Num instance
   , testProperty "fromInteger" $ 
       \x -> toRational (fromInteger x :: FMPQ) == toRational x
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
       \x -> let
       (q,r) = properFraction (fromRational x :: FMPQ)
       (q',r') = properFraction x
       in q == q' && toRational r == r'
   ] 
