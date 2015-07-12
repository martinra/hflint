module Primes.UnitTests
where


import Test.Tasty ( testGroup, TestTree )
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit as HU ( (@?=) )

import HFlint.Primes


primesUnitTests :: TestTree
primesUnitTests = testGroup "comparing to Sage result"
  [ HU.testCase "take 50 $ primesAfter 2^16" $ 
      ( take 50 $ primesAfter 0XFFFF )
      @?=
      [ 65537, 65539, 65543, 65551, 65557,
        65563, 65579, 65581, 65587, 65599,
        65609, 65617, 65629, 65633, 65647,
        65651, 65657, 65677, 65687, 65699,
        65701, 65707, 65713, 65717, 65719,
        65729, 65731, 65761, 65777, 65789,
        65809, 65827, 65831, 65837, 65839,
        65843, 65851, 65867, 65881, 65899,
        65921, 65927, 65929, 65951, 65957,
        65963, 65981, 65983, 65993, 66029
      ]
  , HU.testCase "takeWhile (<100) $ primesAfter 1" $ 
      ( takeWhile (<100) $ primesAfter 1 )
      @?=
      [ 2,  3,  5,  7,  11, 13, 17, 19, 23, 29,
        31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
        73, 79, 83, 89, 97
      ]
  ]
