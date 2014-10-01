module Flint.FMPQMat.Internal
where


import Flint.Internal.Flint

import Flint.FMPQ.FFI
import Flint.FMPQ.Internal

import Flint.FMPQMat.FFI

import Foreign.Ptr (Ptr)


withFMPQMat :: FMPQMat -> (Ptr CFMPQMatType -> Ptr CFMPQMat -> IO b) -> IO (FMPQMat, b)
withFMPQMat = withFlint

withFMPQMat_ :: FMPQMat -> (Ptr CFMPQMatType -> Ptr CFMPQMat -> IO b) -> IO FMPQMat
withFMPQMat_ = withFlint_

withNewFMPQMat :: Int -> Int -> (Ptr CFMPQMatType -> Ptr CFMPQMat -> IO b) -> IO (FMPQMat, b)
withNewFMPQMat r c = withNewFlint (FMPQType, fromIntegral r, fromIntegral c)

withNewFMPQMat_ :: Int -> Int -> (Ptr CFMPQMatType -> Ptr CFMPQMat -> IO b) -> IO FMPQMat
withNewFMPQMat_ r c = withNewFlint_ (FMPQType, fromIntegral r, fromIntegral c)


