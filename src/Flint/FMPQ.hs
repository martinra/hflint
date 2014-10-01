{-# LANGUAGE
    FlexibleInstances
  , DeriveDataTypeable
  , GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  #-}

module Flint.FMPQ
    ( FMPQ
    , withFMPQ
    , withFMPQ_
    , withNewFMPQ
    , withNewFMPQ_
    )
where

import Flint.Internal.FlintCalls
import Flint.FMPZ
import Flint.FMPZ.FFI
import Flint.FMPQ.FFI

import Data.Ratio (numerator, denominator)

import Foreign.C.String (peekCString)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal (free)

import System.IO.Unsafe (unsafePerformIO)


withFMPQ :: FMPQ -> (Ptr CFMPQ -> IO b) -> IO (FMPQ, b)
withFMPQ = withFlint

withFMPQ_ :: FMPQ -> (Ptr CFMPQ -> IO b) -> IO FMPQ
withFMPQ_ = withFlint_

withNewFMPQ :: (Ptr CFMPQ -> IO b) -> IO (FMPQ, b)
withNewFMPQ = withNewFlint

withNewFMPQ_ :: (Ptr CFMPQ -> IO b) -> IO FMPQ
withNewFMPQ_ = withNewFlint_


instance Show FMPQ where
    show = flip toString 10

-- todo: use other show function ??
toString :: FMPQ -> Int -> String
toString a base = unsafePerformIO $ do
  cstr <- lift0Flint (fmpq_get_str nullPtr (fromIntegral base)) a
  str <- peekCString cstr
  free cstr
  return str


instance Num FMPQ where
    fromInteger a = unsafePerformIO $
                    withNewFlint_ $ \cptr ->
                    withFMPZ_ (fromInteger a) $ \aptr ->
                    withNewFMPZ $ \bptr -> do
                      fmpz_one bptr
                      fmpq_set_fmpz_frac cptr aptr bptr
    (+) = lift2Flint_ fmpq_add
    (-) = lift2Flint_ fmpq_sub
    (*) = lift2Flint_ fmpq_mul
    abs = liftFlint_ fmpq_abs
    signum = fromInteger . fromIntegral . unsafePerformIO . lift0Flint fmpq_sgn
    negate = liftFlint_ fmpq_neg


instance Fractional FMPQ where
    fromRational a = unsafePerformIO $
                     withNewFlint_ $ \cptr ->
                     withFMPZ (fromInteger num) $ \aptr ->
                     withFMPZ (fromInteger den) $ \bptr ->
                     fmpq_set_fmpz_frac cptr aptr bptr
        where
          num = numerator a
          den = denominator a

    (/) = lift2Flint_ fmpq_div
    recip = liftFlint_ fmpq_inv
