module HFlint.Test.Utility.FMPQPoly
where

import HFlint.FMPQPoly
import qualified HFlint.Test.Utility.Intertwine as U


-- We need to specify the type, so that a is not specialized when infering the
-- type on first occurence of equal and equal2
equal :: Eq a => ([Rational] -> a) -> (FMPQPoly -> a) -> [Rational] -> Bool
equal2 :: Eq a => ([Rational] -> [Rational] -> a) -> (FMPQPoly -> FMPQPoly -> a) -> [Rational] -> [Rational] -> Bool
equal         = U.equal (fromRationals :: [Rational] -> FMPQPoly) toRationals
equal2        = U.equal2 (fromRationals :: [Rational] -> FMPQPoly) toRationals
intertwining  = U.intertwining (fromRationals :: [Rational] -> FMPQPoly) toRationals
intertwining2 = U.intertwining2 (fromRationals :: [Rational] -> FMPQPoly) toRationals
