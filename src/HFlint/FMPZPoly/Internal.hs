{-# LANGUAGE
    TypeFamilies
  #-}

module HFlint.FMPZPoly.Internal
where

import Control.Monad ( (>=>) )
import Foreign.ForeignPtr ( mallocForeignPtr
                          , withForeignPtr
                          , addForeignPtrFinalizer
                          )
import Foreign.Ptr ( Ptr )

import HFlint.FMPZPoly.FFI
import HFlint.Internal.Flint


instance Flint FMPZPoly where
  type CFlint FMPZPoly = CFMPZPoly
  type FlintType FMPZPoly = FMPZPolyType
  type CFlintType FMPZPoly = CFMPZPolyType

  flintType _ = FMPZPolyType

  newFlint _ = do
    a <- mallocForeignPtr
    withForeignPtr a fmpz_poly_init
    addForeignPtrFinalizer p_fmpz_poly_clear a
    return $ FMPZPoly a

  withFlint (FMPZPoly a) f = withForeignPtr a $
                             f undefined >=> \r -> return (FMPZPoly a,r)

withFMPZPoly :: FMPZPoly -> (Ptr CFMPZPolyType -> Ptr CFMPZPoly -> IO b)
         -> IO (FMPZPoly, b)
withFMPZPoly = withFlint

withFMPZPoly_ :: FMPZPoly -> (Ptr CFMPZPolyType -> Ptr CFMPZPoly -> IO b)
          -> IO FMPZPoly
withFMPZPoly_ = withFlint_

withNewFMPZPoly :: (Ptr CFMPZPolyType -> Ptr CFMPZPoly -> IO b)
            -> IO (FMPZPoly, b)
withNewFMPZPoly = withNewFlint FMPZPolyType

withNewFMPZPoly_ :: (Ptr CFMPZPolyType -> Ptr CFMPZPoly -> IO b)
             -> IO FMPZPoly
withNewFMPZPoly_ = withNewFlint_ FMPZPolyType
