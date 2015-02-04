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

  flintType _ = FMPZType

  newFlint _ = do
    a <- mallocForeignPtr
    withForeignPtr a fmpz_init
    addForeignPtrFinalizer p_fmpz_clear a
    return $ FMPZ a

  withFlint (FMPZ a) f = withForeignPtr a $
                         f undefined >=> \r -> return (FMPZ a,r)


withFMPZ :: FMPZ -> (Ptr CFMPZType -> Ptr CFMPZ -> IO b)
         -> IO (FMPZ, b)
withFMPZ = withFlint 

withFMPZ_ :: FMPZ -> (Ptr CFMPZType -> Ptr CFMPZ -> IO b)
          -> IO FMPZ
withFMPZ_ = withFlint_

withNewFMPZ :: (Ptr CFMPZType -> Ptr CFMPZ -> IO b)
            -> IO (FMPZ, b)
withNewFMPZ = withNewFlint FMPZType

withNewFMPZ_ :: (Ptr CFMPZType -> Ptr CFMPZ -> IO b)
             -> IO FMPZ
withNewFMPZ_ = withNewFlint_ FMPZType
