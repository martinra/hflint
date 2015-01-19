module Flint.FMPZ.Internal
where


import Flint.Internal.Flint

import Flint.FMPZ.FFI

import Foreign.Ptr (Ptr)


withFMPZ :: FMPZ -> (Ptr CFMPZType -> Ptr CFMPZ -> IO b) -> IO (FMPZ, b)
withFMPZ = withFlint 

withFMPZ_ :: FMPZ -> (Ptr CFMPZType -> Ptr CFMPZ -> IO b) -> IO FMPZ
withFMPZ_ = withFlint_

withNewFMPZ :: (Ptr CFMPZType -> Ptr CFMPZ -> IO b) -> IO (FMPZ, b)
withNewFMPZ = withNewFlint FMPZType

withNewFMPZ_ :: (Ptr CFMPZType -> Ptr CFMPZ -> IO b) -> IO FMPZ
withNewFMPZ_ = withNewFlint_ FMPZType

