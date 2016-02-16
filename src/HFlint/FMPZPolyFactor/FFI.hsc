{-# LANGUAGE
    CApiFFI
  , EmptyDataDecls
  , FlexibleInstances
  , ForeignFunctionInterface
  , MultiParamTypeClasses
  , TupleSections
  , TypeFamilies
  #-}

module HFlint.FMPZPolyFactor.FFI
where

#include <flint/fmpz_poly.h>

import Foreign.C.Types ( CLong(..) )
import Foreign.ForeignPtr ( ForeignPtr
                          , mallocForeignPtr, withForeignPtr
                          , addForeignPtrFinalizer
                          )
import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.Storable ( Storable(..) )

import HFlint.FMPZ.FFI
import HFlint.FMPZPoly.FFI
import HFlint.Internal.Flint


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


data FMPZPolyFactor = FMPZPolyFactor {-# UNPACK #-} !(ForeignPtr CFMPZPolyFactor)
type CFMPZPolyFactor = CFlint FMPZPolyFactor

instance Flint FMPZPolyFactor where
  data CFlint FMPZPolyFactor

  newFlint = do
    a <- mallocForeignPtr
    withForeignPtr a fmpz_poly_factor_init
    addForeignPtrFinalizer p_fmpz_poly_factor_clear a
    return $ FMPZPolyFactor a

  withFlint (FMPZPolyFactor a) f =
    withForeignPtr a $ \aptr ->
    f aptr >>= return . (FMPZPolyFactor a,)


withFMPZPolyFactor
  :: FMPZPolyFactor -> (Ptr CFMPZPolyFactor -> IO b)
  -> IO (FMPZPolyFactor, b)
withFMPZPolyFactor = withFlint

withFMPZPolyFactor_
  :: FMPZPolyFactor -> (Ptr CFMPZPolyFactor -> IO b)
  -> IO FMPZPolyFactor
withFMPZPolyFactor_ = withFlint_

withNewFMPZPolyFactor
  :: (Ptr CFMPZPolyFactor -> IO b)
  -> IO (FMPZPolyFactor, b)
withNewFMPZPolyFactor = withNewFlint

withNewFMPZPolyFactor_
  :: (Ptr CFMPZPolyFactor -> IO b)
  -> IO FMPZPolyFactor
withNewFMPZPolyFactor_ = withNewFlint_


instance Storable CFMPZPolyFactor where
    sizeOf _ = #size fmpz_poly_factor_struct
    alignment _ = #alignment fmpz_poly_factor_struct
    peek = error "CFMPZPolyFactor.peek: Not defined"
    poke = error "CFMPZPolyFactor.poke: Not defined"


foreign import ccall unsafe "fmpz_poly_factor_init"
        fmpz_poly_factor_init :: Ptr CFMPZPolyFactor -> IO ()

foreign import ccall unsafe "fmpz_poly_factor_init2"
        fmpz_poly_factor_init2 :: Ptr CFMPZPolyFactor -> CLong -> IO ()

foreign import ccall unsafe "fmpz_poly_factor_clear"
        fmpz_poly_factor_clear :: Ptr CFMPZPolyFactor -> IO ()

foreign import capi "flint/fmpz_poly.h value fmpz_poly_factor_clear"
        p_fmpz_poly_factor_clear :: FunPtr (Ptr CFMPZPolyFactor -> IO ())

foreign import ccall unsafe "fmpz_poly_factor_zassenhaus"
        fmpz_poly_factor_zassenhaus :: Ptr CFMPZPolyFactor -> Ptr CFMPZPoly -> IO ()


foreign import ccall unsafe "fmpz_poly_factor_get_content_additional"
        fmpz_poly_factor_get_content :: Ptr CFMPZ -> Ptr CFMPZPolyFactor -> IO ()

foreign import ccall unsafe "fmpz_poly_factor_number_factors_additional"
        fmpz_poly_factor_number_factors :: Ptr CFMPZPolyFactor -> IO CLong

foreign import ccall unsafe "fmpz_poly_factor_get_factor_additional"
        fmpz_poly_factor_get_factor :: Ptr CFMPZPoly -> Ptr CFMPZPolyFactor -> CLong -> IO CLong
