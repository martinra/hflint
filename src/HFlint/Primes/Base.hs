module HFlint.Primes.Base
where

import System.IO.Unsafe ( unsafePerformIO )

import HFlint.NMod.FFI ( FlintLimb )
import HFlint.Primes.FFI


-- largest prime that fits into Word64 is 2^64-59 = 0XFFFFFFFFFFFFFFC5
primesAfter :: FlintLimb -> [FlintLimb ]
primesAfter n = 
  let p = nextPrime n
  in if p == 0XFFFFFFFFFFFFFFC5
     then [p]
     else p:primesAfter p

nextPrime :: FlintLimb -> FlintLimb 
nextPrime n = unsafePerformIO $ n_nextprime n 1
