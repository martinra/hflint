module HFlint.Test.Utility.FMPQ
where

import HFlint.FMPQ
import qualified HFlint.Test.Utility.Intertwine as U


-- We need to specify the type, so that a is not specialized when infering the
-- type on first occurence of equal and equal2
equal :: Eq a => (Rational -> a) -> (FMPQ -> a) -> Rational -> Bool
equal2 :: Eq a => (Rational -> Rational -> a) -> (FMPQ -> FMPQ -> a) -> Rational -> Rational -> Bool
equal         = U.equal (fromRational :: Rational -> FMPQ) toRational
equal2        = U.equal2 (fromRational :: Rational -> FMPQ) toRational
intertwining  = U.intertwining (fromRational :: Rational -> FMPQ) toRational
intertwining2 = U.intertwining2 (fromRational :: Rational -> FMPQ) toRational
