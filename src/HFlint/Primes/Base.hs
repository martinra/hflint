module HFlint.Primes.Base
where

import Data.Word ( Word64 )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.Primes.FFI


-- largest prime that fits into Word64 is 2^64-59 = 0XFFFFFFFFFFFFFFC5
primesAfter :: Word64 -> [Word64]
primesAfter n = 
  let p = nextPrime n
  in if p == 0XFFFFFFFFFFFFFFC5
     then [p]
     else p:primesAfter p

nextPrime :: Word64 -> Word64
nextPrime n = unsafePerformIO $
  fromIntegral <$> n_nextprime (fromIntegral n) 1
