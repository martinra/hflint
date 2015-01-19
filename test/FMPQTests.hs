module FMPQTests
where

import Test.Tasty ( testGroup ) 

import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU

import HFlint.FMPQ ()


testGroup :: TestTree
testGruop = testGroup "FMPQ Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

intertwining :: (Rational -> Rational)
             -> (FMPQ -> FMPQ)
             -> Rational
             -> Bool
intertwining f g x =
  f x == toRational $ g $ fromRational a

intertwining2 :: (Rational -> Rational -> Rational)
              -> (FMPQ -> FMPQ -> FMPQ)
              -> Rational -> Rational
              -> Bool
intertwining2 f g x y =
  f x y == toRational $ g (fromRational x) (fromRational y)

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "toRational . fromRational == const" $ intertwining id id
  , QC.testProperty "Addition" $ intertwining2 (+) (+)
  , QC.testProperty "Substraction" $ intertwining2 (-) (-)
  , QC.testProperty "Negation" $ intertwining negate negate
  , QC.testProperty "Multiplication" $ intertwining2 (*) (*)
  , QC.testProperty "Absolute Value" $ intertwining signum signum

  , QC.testProperty "Show" $ intertwining show show

  , QC.testProperty "Devision" $ intertwining2 (/) (/)
  , QC.testProperty "Reciprocal" $ intertwining recip recip
  ]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "toRational . fromRational == const" $ intertwining id id
  , SC.testProperty "Addition" $ intertwining2 (+) (+)
  , SC.testProperty "Substraction" $ intertwining2 (-) (-)
  , SC.testProperty "Negation" $ intertwining negate negate
  , SC.testProperty "Multiplication" $ intertwining2 (*) (*)
  , SC.testProperty "Absolute Value" $ intertwining signum signum

  , SC.testProperty "Show" $ intertwining show show

  , SC.testProperty "Devision" $ intertwining2 (/) (/)
  , SC.testProperty "Reciprocal" $ intertwining recip recip
  ]

-- unitTests = testGroup "Unit tests"
--   [ testCase "List comparison (different length)" $
--       [1, 2, 3] `compare` [1,2] @?= GT
-- 
--   -- the following test does not hold
--   , testCase "List comparison (same length)" $
--       [1, 2, 3] `compare` [1,2,2] @?= LT
--   ]
