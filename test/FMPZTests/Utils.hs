module FMPZTests.Utils
where

import HFlint.FMPZ
import qualified TestHFlint.Utils as U


-- We need to specify the type, so that a is not specialized when infering the
-- type on first occurence of equal and equal2
equal :: Eq a => (Integer -> a) -> (FMPZ -> a) -> Integer -> Bool
equal2 :: Eq a => (Integer -> Integer -> a) -> (FMPZ -> FMPZ -> a) -> Integer -> Integer -> Bool
equal         = U.equal (fromInteger :: Integer -> FMPZ) toInteger
equal2        = U.equal2 (fromInteger :: Integer -> FMPZ) toInteger
intertwining  = U.intertwining (fromInteger :: Integer -> FMPZ) toInteger
intertwining2 = U.intertwining2 (fromInteger :: Integer -> FMPZ) toInteger


