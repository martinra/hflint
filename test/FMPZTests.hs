module FMPZTests
where

import Test.Tasty ( testGroup ) 

import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU

import HFlint.FMPZ ()


testGroup :: TestTree
testGruop = testGroup "FMPZ Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

intertwining :: (Integer -> Integer)
             -> (FMPZ -> FMPZ)
             -> Integer
             -> Bool
intertwining f g x =
  f x == toIntegral $ g $ fromInteger x

intertwining2 :: (Integer -> Integer -> Integer)
              -> (FMPZ -> FMPZ -> FMPZ)
              -> Integer -> Integer
              -> Bool
intertwining2 f g x y =
  f x y == toIntegral $ g (fromInteger x) (fromInteger y)

qcProps = testGroup "(checked by QuickCheck)"
  [
    -- Show instance
    QC.testProperty "Show" $ intertwining show show

    -- Num instance
  , QC.testProperty "toIntegral . fromInteger == const" $ intertwining id id
  , QC.testProperty "Addition" $ intertwining2 (+) (+)
  , QC.testProperty "Substraction" $ intertwining2 (-) (-)
  , QC.testProperty "Negation" $ intertwining negate negate
  , QC.testProperty "Multiplication" $ intertwining2 (*) (*)
  , QC.testProperty "Absolute Value" $ intertwining signum signum
  ]

scProps = testGroup "(checked by SmallCheck)"
  [
    -- Show instance
    SC.testProperty "Show" $ intertwining show show

    -- Num instance
  , SC.testProperty "toIntegral . fromInteger == const" $ intertwining id id
  , SC.testProperty "Addition" $ intertwining2 (+) (+)
  , SC.testProperty "Substraction" $ intertwining2 (-) (-)
  , SC.testProperty "Negation" $ intertwining negate negate
  , SC.testProperty "Multiplication" $ intertwining2 (*) (*)
  , SC.testProperty "Absolute Value" $ intertwining signum signum
  ]

-- unitTests = testGroup "Unit tests"
--   [ testCase "List comparison (different length)" $
--       [1, 2, 3] `compare` [1,2] @?= GT
-- 
--   -- the following test does not hold
--   , testCase "List comparison (same length)" $
--       [1, 2, 3] `compare` [1,2,2] @?= LT
--   ]
