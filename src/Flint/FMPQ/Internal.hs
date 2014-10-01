module Flint.FMPQ.Internal
where

import Flint.Internal.Flint
import Flint.FMPQ.FFI

import Foreign.Ptr (Ptr)


withFMPQ :: FMPQ -> (Ptr CFMPQType -> Ptr CFMPQ -> IO b) -> IO (FMPQ, b)
withFMPQ = withFlint

withFMPQ_ :: FMPQ -> (Ptr CFMPQType -> Ptr CFMPQ -> IO b) -> IO FMPQ
withFMPQ_ = withFlint_

withNewFMPQ :: (Ptr CFMPQType -> Ptr CFMPQ -> IO b) -> IO (FMPQ, b)
withNewFMPQ = withNewFlint FMPQType

withNewFMPQ_ :: (Ptr CFMPQType -> Ptr CFMPQ -> IO b) -> IO FMPQ
withNewFMPQ_ = withNewFlint_ FMPQType

