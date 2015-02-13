{-# LANGUAGE
    TypeFamilies
  #-}

module HFlint.FMPQ.Internal
where

import Control.Monad ( (>=>) )
import Foreign.ForeignPtr ( mallocForeignPtr
                          , withForeignPtr
                          , addForeignPtrFinalizer
                          )
import Foreign.Ptr ( Ptr )

import HFlint.FMPQ.FFI
import HFlint.Internal.Flint


instance Flint FMPQ where
  type CFlint FMPQ = CFMPQ
  type FlintType FMPQ = FMPQType
  type CFlintType FMPQ = CFMPQType

  flintType _ = FMPQType

  newFlint _ = do
    a <- mallocForeignPtr
    withForeignPtr a fmpq_init
    addForeignPtrFinalizer p_fmpq_clear a
    return $ FMPQ a

  withFlint (FMPQ a) f = withForeignPtr a $
                         f undefined >=> \r -> return (FMPQ a,r)

withFMPQ :: FMPQ -> (Ptr CFMPQType -> Ptr CFMPQ -> IO b)
         -> IO (FMPQ, b)
withFMPQ = withFlint

withFMPQ_ :: FMPQ -> (Ptr CFMPQType -> Ptr CFMPQ -> IO b)
          -> IO FMPQ
withFMPQ_ = withFlint_

withNewFMPQ :: (Ptr CFMPQType -> Ptr CFMPQ -> IO b)
            -> IO (FMPQ, b)
withNewFMPQ = withNewFlint FMPQType

withNewFMPQ_ :: (Ptr CFMPQType -> Ptr CFMPQ -> IO b)
             -> IO FMPQ
withNewFMPQ_ = withNewFlint_ FMPQType
