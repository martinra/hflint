module Flint.FMPQ.Basic
where

import Flint.Internal.Flint

import Flint.FMPQ.Internal
import Flint.FMPQ.FFI

import Foreign.C.String (peekCString)
import Foreign.Ptr (nullPtr)
import Foreign.Marshal (free)

import System.IO.Unsafe (unsafePerformIO)

instance Show FMPQ where
    show = flip toString 10

-- todo: use other show function ??
toString :: FMPQ -> Int -> String
toString a base = unsafePerformIO $ do
  cstr <- lift0Flint (const $ fmpq_get_str nullPtr (fromIntegral base)) a
  str <- peekCString cstr
  free cstr
  return str


