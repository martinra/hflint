{-# LANGUAGE
    ForeignFunctionInterface
  #-}

module HFlint.Primes.FFI
where

#include <flint/ulong_extras.h>

import Foreign.C.Types ( CULong(..), CInt(..) )


foreign import ccall unsafe "n_nextprime"
  n_nextprime :: CULong -> CInt -> IO CULong
