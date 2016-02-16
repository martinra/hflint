{-# LANGUAGE
    CApiFFI
  , EmptyDataDecls
  , FlexibleInstances
  , ForeignFunctionInterface
  , MultiParamTypeClasses
  , TupleSections
  , TypeFamilies
  #-}

module HFlint.FMPZPoly.FFI
where

#include <flint/fmpz_poly.h>

import Foreign.C.String ( CString )
import Foreign.C.Types ( CInt(..)
                       , CLong(..)
                       )
import Foreign.ForeignPtr ( ForeignPtr
                          , mallocForeignPtr, withForeignPtr
                          , addForeignPtrFinalizer
                          )
import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.Storable ( Storable(..) )

import HFlint.FMPZ.FFI
import HFlint.Internal.Flint


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


data FMPZPoly = FMPZPoly {-# UNPACK #-} !(ForeignPtr CFMPZPoly)
type CFMPZPoly = CFlint FMPZPoly

instance Flint FMPZPoly where
  data CFlint FMPZPoly

  newFlint = do
    a <- mallocForeignPtr
    withForeignPtr a fmpz_poly_init
    addForeignPtrFinalizer p_fmpz_poly_clear a
    return $ FMPZPoly a

  withFlint (FMPZPoly a) f =
    withForeignPtr a $ \aptr ->
    f aptr >>= return . (FMPZPoly a,)

withFMPZPoly :: FMPZPoly -> (Ptr CFMPZPoly -> IO b) -> IO (FMPZPoly, b)
withFMPZPoly = withFlint

withFMPZPoly_ :: FMPZPoly -> (Ptr CFMPZPoly -> IO b) -> IO FMPZPoly
withFMPZPoly_ = withFlint_

withNewFMPZPoly :: (Ptr CFMPZPoly -> IO b) -> IO (FMPZPoly, b)
withNewFMPZPoly = withNewFlint

withNewFMPZPoly_ :: (Ptr CFMPZPoly -> IO b) -> IO FMPZPoly
withNewFMPZPoly_ = withNewFlint_


instance Storable CFMPZPoly where
    sizeOf _ = #size fmpz_poly_struct
    alignment _ = #alignment fmpz_poly_struct
    peek = error "CFMPZPoly.peek: Not defined"
    poke = error "CFMPZPoly.poke: Not defined"


foreign import ccall unsafe "fmpz_poly_init"
        fmpz_poly_init :: Ptr CFMPZPoly -> IO ()

foreign import ccall unsafe "fmpz_poly_init2"
        fmpz_poly_init2 :: Ptr CFMPZPoly -> CLong -> IO ()

foreign import ccall unsafe "fmpz_poly_realloc"
        fmpz_poly_realloc :: Ptr CFMPZPoly -> CLong -> IO ()

foreign import ccall unsafe "fmpz_poly_clear"
        fmpz_poly_clear :: Ptr CFMPZPoly -> IO ()

foreign import capi "flint/fmpz_poly.h value fmpz_poly_clear"
        p_fmpz_poly_clear :: FunPtr (Ptr CFMPZPoly -> IO ())


foreign import capi unsafe "flint/fmpz_poly.h fmpz_poly_degree"
        fmpz_poly_degree :: Ptr CFMPZPoly -> IO CLong

foreign import ccall unsafe "fmpz_poly_set_fmpz"
        fmpz_poly_set_fmpz :: Ptr CFMPZPoly -> Ptr CFMPZ -> IO ()


foreign import ccall unsafe "fmpz_poly_get_str"
        fmpz_poly_get_str :: Ptr CFMPZPoly -> IO CString

foreign import ccall unsafe "fmpz_poly_get_str_pretty"
        fmpz_poly_get_str_pretty :: Ptr CFMPZPoly -> CString -> IO CString


foreign import capi unsafe "flint/fmpz_poly.h fmpz_poly_truncate"
        fmpz_poly_truncate :: Ptr CFMPZPoly -> CLong -> IO ()       

        
foreign import capi unsafe "flint/fmpz_poly.h fmpz_poly_zero"
        fmpz_poly_zero :: Ptr CFMPZPoly -> IO ()

foreign import capi unsafe "flint/fmpz_poly.h fmpz_poly_one"
        fmpz_poly_one :: Ptr CFMPZPoly -> IO ()


foreign import ccall unsafe "fmpz_poly_get_coeff_fmpz"
        fmpz_poly_get_coeff_fmpz :: Ptr CFMPZ -> Ptr CFMPZPoly -> CLong -> IO ()

foreign import ccall unsafe "fmpz_poly_set_coeff_fmpz"
        fmpz_poly_set_coeff_fmpz :: Ptr CFMPZPoly -> CLong -> Ptr CFMPZ -> IO ()


foreign import ccall unsafe "fmpz_poly_equal"
        fmpz_poly_equal :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> IO CInt

foreign import ccall unsafe "fmpz_poly_is_zero_wrapper"
        fmpz_poly_is_zero :: Ptr CFMPZPoly -> IO CInt

foreign import capi unsafe "flint/fmpz_poly.h fmpz_poly_is_one"
        fmpz_poly_is_one :: Ptr CFMPZPoly -> IO CInt


foreign import ccall unsafe "fmpz_poly_add"
        fmpz_poly_add :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> Ptr CFMPZPoly -> IO ()

foreign import ccall unsafe "fmpz_poly_sub"
        fmpz_poly_sub :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> Ptr CFMPZPoly -> IO ()

foreign import ccall unsafe "fmpz_poly_neg"
        fmpz_poly_neg :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> IO ()       


foreign import ccall unsafe "fmpz_poly_scalar_mul_fmpz"
        fmpz_poly_scalar_mul_fmpz :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> Ptr CFMPZ -> IO ()

foreign import ccall unsafe "fmpz_poly_scalar_fdiv_fmpz"
        fmpz_poly_scalar_fdiv_fmpz :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> Ptr CFMPZ -> IO ()

foreign import ccall unsafe "fmpz_poly_scalar_tdiv_fmpz"
        fmpz_poly_scalar_tdiv_fmpz :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> Ptr CFMPZ -> IO ()

foreign import ccall unsafe "fmpz_poly_scalar_divexact_fmpz"
        fmpz_poly_scalar_divexact_fmpz :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> Ptr CFMPZ -> IO ()


foreign import ccall unsafe "fmpz_poly_mul"
        fmpz_poly_mul :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> Ptr CFMPZPoly -> IO ()


foreign import ccall unsafe "fmpz_poly_shift_left"
        fmpz_poly_shift_left :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> CLong -> IO ()

foreign import ccall unsafe "fmpz_poly_shift_right"
        fmpz_poly_shift_right :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> CLong -> IO ()




foreign import ccall unsafe "fmpz_poly_gcd"
        fmpz_poly_gcd :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> Ptr CFMPZPoly -> IO ()

foreign import ccall unsafe "fmpz_poly_lcm"
        fmpz_poly_lcm :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> Ptr CFMPZPoly -> IO ()

foreign import capi unsafe "flint/fmpz_poly.h fmpz_poly_xgcd"
        fmpz_poly_xgcd :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> Ptr CFMPZPoly -> Ptr CFMPZPoly -> Ptr CFMPZPoly -> IO ()


foreign import ccall unsafe "fmpz_poly_content"
        fmpz_poly_content :: Ptr CFMPZ -> Ptr CFMPZPoly -> IO ()

foreign import ccall unsafe "fmpz_poly_primitive_part"
        fmpz_poly_primitive_part :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> IO ()


foreign import ccall unsafe "fmpz_poly_divrem"
        fmpz_poly_divrem :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> Ptr CFMPZPoly -> Ptr CFMPZPoly -> IO ()

foreign import ccall unsafe "fmpz_poly_div"
        fmpz_poly_div :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> Ptr CFMPZPoly -> IO ()

foreign import ccall unsafe "fmpz_poly_rem"
        fmpz_poly_rem :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> Ptr CFMPZPoly -> IO ()


foreign import capi unsafe "flint/fmpz_poly.h fmpz_poly_inv_series"
        fmpz_poly_inv_series :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> CLong -> IO ()

foreign import ccall unsafe "fmpz_poly_div_series"
        fmpz_poly_div_series :: Ptr CFMPZPoly -> Ptr CFMPZPoly -> Ptr CFMPZPoly -> CLong -> IO ()
