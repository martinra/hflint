{-# LANGUAGE
    TypeFamilies
  #-}

module HFlint.FMPQPoly.Internal
where

import Control.Monad ( (>=>) )
import Foreign.ForeignPtr ( mallocForeignPtr
                          , withForeignPtr
                          , addForeignPtrFinalizer
                          )
import Foreign.Ptr ( Ptr )

import HFlint.FMPQPoly.FFI
import HFlint.Internal.Flint


instance Flint FMPQPoly where
  type CFlint FMPQPoly = CFMPQPoly
  type FlintType FMPQPoly = FMPQPolyType
  type CFlintType FMPQPoly = CFMPQPolyType

  flintType _ = FMPQPolyType

  newFlint _ = do
    a <- mallocForeignPtr
    withForeignPtr a fmpq_poly_init
    addForeignPtrFinalizer p_fmpq_poly_clear a
    return $ FMPQPoly a

  withFlint (FMPQPoly a) f = withForeignPtr a $
                             f undefined >=> \r -> return (FMPQPoly a,r)

withFMPQPoly :: FMPQPoly -> (Ptr CFMPQPolyType -> Ptr CFMPQPoly -> IO b)
         -> IO (FMPQPoly, b)
withFMPQPoly = withFlint

withFMPQPoly_ :: FMPQPoly -> (Ptr CFMPQPolyType -> Ptr CFMPQPoly -> IO b)
          -> IO FMPQPoly
withFMPQPoly_ = withFlint_

withNewFMPQPoly :: (Ptr CFMPQPolyType -> Ptr CFMPQPoly -> IO b)
            -> IO (FMPQPoly, b)
withNewFMPQPoly = withNewFlint FMPQPolyType

withNewFMPQPoly_ :: (Ptr CFMPQPolyType -> Ptr CFMPQPoly -> IO b)
             -> IO FMPQPoly
withNewFMPQPoly_ = withNewFlint_ FMPQPolyType
