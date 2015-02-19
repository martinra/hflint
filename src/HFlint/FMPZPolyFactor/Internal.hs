{-# LANGUAGE
    TypeFamilies
  #-}

module HFlint.FMPZPolyFactor.Internal
where

import Control.Monad ( (>=>) )
import Foreign.ForeignPtr ( mallocForeignPtr
                          , withForeignPtr
                          , addForeignPtrFinalizer
                          )
import Foreign.Ptr ( Ptr )

import HFlint.FMPZPoly.Internal
import HFlint.FMPZPolyFactor.FFI
import HFlint.Internal.Flint


instance Flint FMPZPolyFactor where
  type CFlint FMPZPolyFactor = CFMPZPolyFactor
  type FlintType FMPZPolyFactor = FMPZPolyFactorType
  type CFlintType FMPZPolyFactor = CFMPZPolyFactorType

  flintType _ = FMPZPolyFactorType

  newFlint _ = do
    a <- mallocForeignPtr
    withForeignPtr a fmpz_poly_factor_init
    addForeignPtrFinalizer p_fmpz_poly_factor_clear a
    return $ FMPZPolyFactor a

  withFlint (FMPZPolyFactor a) f = withForeignPtr a $
                                   f undefined >=> \r -> return (FMPZPolyFactor a,r)

withFMPZPolyFactor :: FMPZPolyFactor -> (Ptr CFMPZPolyFactorType -> Ptr CFMPZPolyFactor -> IO b)
         -> IO (FMPZPolyFactor, b)
withFMPZPolyFactor = withFlint

withFMPZPolyFactor_ :: FMPZPolyFactor -> (Ptr CFMPZPolyFactorType -> Ptr CFMPZPolyFactor -> IO b)
          -> IO FMPZPolyFactor
withFMPZPolyFactor_ = withFlint_

withNewFMPZPolyFactor :: (Ptr CFMPZPolyFactorType -> Ptr CFMPZPolyFactor -> IO b)
            -> IO (FMPZPolyFactor, b)
withNewFMPZPolyFactor = withNewFlint FMPZPolyFactorType

withNewFMPZPolyFactor_ :: (Ptr CFMPZPolyFactorType -> Ptr CFMPZPolyFactor -> IO b)
             -> IO FMPZPolyFactor
withNewFMPZPolyFactor_ = withNewFlint_ FMPZPolyFactorType
