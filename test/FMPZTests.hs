module FMPZTests
where

import Test.Tasty ( testGroup,
                    TestTree
                  )
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU

import HFlint.FMPZ


fmpzTestGroup :: TestTree
fmpzTestGroup = testGroup "FMPZ Tests" [properties, unitTests]


equal :: Eq a
      => (Integer -> a)
      -> (FMPZ -> a)
      -> Integer
      -> Bool
equal f g x = f x == (g $ fromInteger x)

intertwining :: (Integer -> Integer)
             -> (FMPZ -> FMPZ)
             -> Integer
             -> Bool
intertwining f g x =
  f x == (toInteger $ g $ fromInteger x)

intertwining2 :: (Integer -> Integer -> Integer)
              -> (FMPZ -> FMPZ -> FMPZ)
              -> Integer -> Integer
              -> Bool
intertwining2 f g x y =
  f x y == (toInteger $ g (fromInteger x) (fromInteger y))


testProperty s p = testGroup ("s " ++ "(QuickCheck & SmallCheck)")
  [ QC.testProperty s p,
    SC.testProperty s p
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ -- Show instance
    testProperty "Show" $ equal show show 

    -- Num instance
  , testProperty "toIntegral . fromInteger == const" $ intertwining id id
  , testProperty "Addition" $ intertwining2 (+) (+)
  , testProperty "Substraction" $ intertwining2 (-) (-)
  , testProperty "Negation" $ intertwining negate negate
  , testProperty "Multiplication" $ intertwining2 (*) (*)
  , testProperty "Absolute Value" $ intertwining signum signum
  ]


unitTests = testGroup "Unit tests" []
--   [ testCase "List comparison (different length)" $
--       [1, 2, 3] `compare` [1,2] @?= GT
-- 
--   -- the following test does not hold
--   , testCase "List comparison (same length)" $
--       [1, 2, 3] `compare` [1,2,2] @?= LT
--   ]
