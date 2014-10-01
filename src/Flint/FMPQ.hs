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

import Flint.Internal.Flint
import Flint.FMPZ
import Flint.FMPZ.FFI
import Flint.FMPQ.FFI

import Data.Ratio (numerator, denominator)

import Foreign.C.String (peekCString)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal (free)

import System.IO.Unsafe (unsafePerformIO)


withFMPQ :: FMPQ -> (Ptr CFMPQType -> Ptr CFMPQ -> IO b) -> IO (FMPQ, b)
withFMPQ = withFlint

withFMPQ_ :: FMPQ -> (Ptr CFMPQType -> Ptr CFMPQ -> IO b) -> IO FMPQ
withFMPQ_ = withFlint_

withNewFMPQ :: (Ptr CFMPQType -> Ptr CFMPQ -> IO b) -> IO (FMPQ, b)
withNewFMPQ = withNewFlint FMPQType

withNewFMPQ_ :: (Ptr CFMPQType -> Ptr CFMPQ -> IO b) -> IO FMPQ
withNewFMPQ_ = withNewFlint_ FMPQType


instance Show FMPQ where
    show = flip toString 10

-- todo: use other show function ??
toString :: FMPQ -> Int -> String
toString a base = unsafePerformIO $ do
  cstr <- lift0Flint (const $ fmpq_get_str nullPtr (fromIntegral base)) a
  str <- peekCString cstr
  free cstr
  return str


instance Num FMPQ where
    fromInteger a = unsafePerformIO $
                    withNewFMPQ_ $ const $ \cptr ->
                    withFMPZ_ (fromInteger a) $ const $ \aptr ->
                    withNewFMPZ $ const $ \bptr -> do
                      fmpz_one bptr
                      fmpq_set_fmpz_frac cptr aptr bptr
    (+) = lift2Flint_ $ const fmpq_add
    (-) = lift2Flint_ $ const fmpq_sub
    (*) = lift2Flint_ $ const fmpq_mul
    abs = liftFlint_ $ const fmpq_abs
    signum = fromInteger . fromIntegral . unsafePerformIO .
             lift0Flint (const fmpq_sgn)
    negate = liftFlint_ $ const fmpq_neg


instance Fractional FMPQ where
    fromRational a = unsafePerformIO $
                     withNewFMPQ_ $ const $ \cptr ->
                     withFMPZ (fromInteger num) $ const $ \aptr ->
                     withFMPZ (fromInteger den) $ const $ \bptr ->
                     fmpq_set_fmpz_frac cptr aptr bptr
        where
          num = numerator a
          den = denominator a

    (/) = lift2Flint_ $ const fmpq_div
    recip = liftFlint_ $ const fmpq_inv
