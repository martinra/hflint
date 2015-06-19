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

  {-# INLINE flintType #-}
  flintType _ = FMPQType

  {-# INLINE newFlint #-}
  newFlint _ = do
    a <- mallocForeignPtr
    withForeignPtr a fmpq_init
    addForeignPtrFinalizer p_fmpq_clear a
    return $ FMPQ a

  {-# INLINE withFlint #-}
  withFlint (FMPQ a) f = withForeignPtr a $
                         f undefined >=> \r -> return (FMPQ a,r)

{-# INLINE withFMPQ #-}
withFMPQ :: FMPQ -> (Ptr CFMPQType -> Ptr CFMPQ -> IO b)
         -> IO (FMPQ, b)
withFMPQ = withFlint

{-# INLINE withFMPQ_ #-}
withFMPQ_ :: FMPQ -> (Ptr CFMPQType -> Ptr CFMPQ -> IO b)
          -> IO FMPQ
withFMPQ_ = withFlint_

{-# INLINE withNewFMPQ #-}
withNewFMPQ :: (Ptr CFMPQType -> Ptr CFMPQ -> IO b)
            -> IO (FMPQ, b)
withNewFMPQ = withNewFlint FMPQType

{-# INLINE withNewFMPQ_ #-}
withNewFMPQ_ :: (Ptr CFMPQType -> Ptr CFMPQ -> IO b)
             -> IO FMPQ
withNewFMPQ_ = withNewFlint_ FMPQType
