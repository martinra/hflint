{-# LANGUAGE
    ForeignFunctionInterface
  #-}

module HFlint.Primes.FFI
where

#include <flint/ulong_extras.h>

import Foreign.C.Types ( CULong(..), CInt(..) )

import HFlint.NMod.FFI ( FlintLimb )


foreign import ccall unsafe "n_nextprime"
  n_nextprime :: FlintLimb -> CInt -> IO FlintLimb
