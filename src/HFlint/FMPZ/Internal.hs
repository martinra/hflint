{-# LANGUAGE
    TypeFamilies
  #-}
module HFlint.FMPZ.Internal
where

import Control.Monad ( (>=>) )
import Foreign.Ptr ( Ptr )
import Foreign.ForeignPtr ( addForeignPtrFinalizer
                          , mallocForeignPtr
                          , withForeignPtr )

import HFlint.Internal.Flint
import HFlint.FMPZ.FFI


instance Flint FMPZ where
  type CFlint FMPZ = CFMPZ
  type FlintType FMPZ = FMPZType
  type CFlintType FMPZ = CFMPZType

  {-# INLINE flintType #-}
  flintType _ = FMPZType

  {-# INLINE newFlint #-}
  newFlint _ = do
    a <- mallocForeignPtr
    withForeignPtr a fmpz_init
    addForeignPtrFinalizer p_fmpz_clear a
    return $ FMPZ a

  {-# INLINE withFlint #-}
  withFlint (FMPZ a) f = withForeignPtr a $
                         f undefined >=> \r -> return (FMPZ a,r)


{-# INLINE withFMPZ #-}
withFMPZ :: FMPZ -> (Ptr CFMPZType -> Ptr CFMPZ -> IO b)
         -> IO (FMPZ, b)
withFMPZ = withFlint 

{-# INLINE withFMPZ_ #-}
withFMPZ_ :: FMPZ -> (Ptr CFMPZType -> Ptr CFMPZ -> IO b)
          -> IO FMPZ
withFMPZ_ = withFlint_

{-# INLINE withNewFMPZ #-}
withNewFMPZ :: (Ptr CFMPZType -> Ptr CFMPZ -> IO b)
            -> IO (FMPZ, b)
withNewFMPZ = withNewFlint FMPZType

{-# INLINE withNewFMPZ_ #-}
withNewFMPZ_ :: (Ptr CFMPZType -> Ptr CFMPZ -> IO b)
             -> IO FMPZ
withNewFMPZ_ = withNewFlint_ FMPZType
