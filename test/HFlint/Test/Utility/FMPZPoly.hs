module HFlint.Test.Utility.FMPZPoly
where

import HFlint.FMPZPoly
import qualified HFlint.Test.Utility.Intertwine as U


-- type on first occurence of equal and equal2
equal :: Eq a => ([Integer] -> a) -> (FMPZPoly -> a) -> [Integer] -> Bool
equal2 :: Eq a => ([Integer] -> [Integer] -> a) -> (FMPZPoly -> FMPZPoly -> a) -> [Integer] -> [Integer] -> Bool
equal         = U.equal (fromIntegers :: [Integer] -> FMPZPoly) toIntegers
equal2        = U.equal2 (fromIntegers :: [Integer] -> FMPZPoly) toIntegers
intertwining  = U.intertwining (fromIntegers :: [Integer] -> FMPZPoly) toIntegers
intertwining2 = U.intertwining2 (fromIntegers :: [Integer] -> FMPZPoly) toIntegers
