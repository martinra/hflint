{-# LANGUAGE
    FlexibleInstances
  , DeriveDataTypeable
  , GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  #-}

module Flint.FMPQ
    ( FMPQ
    )
where

import Flint.Internal.FlintCalls
import Flint.FMPZ
import Flint.FMPZ.FFI
import Flint.FMPQ.FFI

import Foreign.C.String (peekCString)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal (free)

import System.IO.Unsafe (unsafePerformIO)


instance Num FMPQ where
    fromInteger a = unsafePerformIO $
                    withNewFlint_ $ \cptr ->
                    (withFlint_ (fromInteger a) :: (Ptr CFMPZ -> IO b) -> IO FMPZ)
                    $ \aptr ->
                    (withNewFlint_ :: (Ptr CFMPZ -> IO b) -> IO FMPZ)
                    $ \bptr -> do
                      fmpz_one bptr
                      fmpq_set_fmpz_frac cptr aptr bptr
    (+) = lift2Flint_ fmpq_add
    (-) = lift2Flint_ fmpq_sub
    (*) = lift2Flint_ fmpq_mul
    abs = liftFlint_ fmpq_abs
    signum = fromInteger . fromIntegral . unsafePerformIO . lift0Flint fmpq_sgn
    negate = liftFlint_ fmpq_neg


instance Show FMPQ where
    show = flip toString 10

-- todo: use other show function ??
toString :: FMPQ -> Int -> String
toString a base = unsafePerformIO $ do
  cstr <- lift0Flint (fmpq_get_str nullPtr (fromIntegral base)) a
  str <- peekCString cstr
  free cstr
  return str
