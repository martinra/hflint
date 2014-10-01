module Flint.FMPZ.Basic
where

import Flint.Internal.Flint

import Flint.FMPZ.FFI

import Foreign.C.String (peekCString)
import Foreign.Ptr (nullPtr)
import Foreign.Marshal (free)

import System.IO.Unsafe (unsafePerformIO)

instance Show FMPZ where
    show = flip toString 10

toString :: FMPZ -> Int -> String
toString a base = unsafePerformIO $ do
  cstr <- lift0Flint (const $ fmpz_get_str nullPtr (fromIntegral base)) a
  str <- peekCString cstr
  free cstr
  return str
