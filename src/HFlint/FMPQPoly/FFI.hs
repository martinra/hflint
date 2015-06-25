{-# LINE 1 "FFI.pre.hsc" #-}
{-# LANGUAGE
{-# LINE 2 "FFI.pre.hsc" #-}
    CApiFFI
  , EmptyDataDecls
  , FlexibleInstances
  , ForeignFunctionInterface
  , MultiParamTypeClasses
  , TupleSections
  , TypeFamilies
  #-}

module HFlint.FMPQPoly.FFI
where


{-# LINE 15 "FFI.pre.hsc" #-}

import Control.Monad.IO.Class ( liftIO )

import Foreign.C.String ( CString )
import Foreign.C.Types ( CInt(..)
                       , CLong(..)
                       )
import Foreign.ForeignPtr ( ForeignPtr
                          , addForeignPtrFinalizer
                          , mallocForeignPtr
                          , withForeignPtr )
import Foreign.Ptr ( Ptr, FunPtr, nullPtr )
import Foreign.Storable ( Storable(..) )

import HFlint.FMPQ.FFI
import HFlint.FMPZ.FFI
import HFlint.FMPZPoly.FFI
import HFlint.Internal.Context
import HFlint.Internal.Flint
import HFlint.Internal.FlintWithContext



{-# LINE 38 "FFI.pre.hsc" #-}


newtype FMPQPoly = FMPQPoly (ForeignPtr CFMPQPoly)
type CFMPQPoly = CFlint FMPQPoly

instance FlintWithContext FlintTrivialContext FMPQPoly where
  data CFlint FMPQPoly

  newFlintCtx = liftIO $ do
    a <- mallocForeignPtr
    withForeignPtr a fmpq_poly_init
    addForeignPtrFinalizer p_fmpq_poly_clear a
    return $ FMPQPoly a

  withFlintCtx (FMPQPoly a) f = liftIO $
    withForeignPtr a $ \aptr ->
    f aptr nullPtr >>= return . (FMPQPoly a,)


instance Flint FMPQPoly

withFMPQPoly :: FMPQPoly -> (Ptr CFMPQPoly -> IO b) -> IO (FMPQPoly, b)
withFMPQPoly = withFlint

withFMPQPoly_ :: FMPQPoly -> (Ptr CFMPQPoly -> IO b) -> IO FMPQPoly
withFMPQPoly_ = withFlint_

withNewFMPQPoly :: (Ptr CFMPQPoly -> IO b) -> IO (FMPQPoly, b)
withNewFMPQPoly = withNewFlint

withNewFMPQPoly_ :: (Ptr CFMPQPoly -> IO b) -> IO FMPQPoly
withNewFMPQPoly_ = withNewFlint_


instance Storable CFMPQPoly where
    sizeOf _ = (32)
{-# LINE 74 "FFI.pre.hsc" #-}
    alignment _ = 8
{-# LINE 75 "FFI.pre.hsc" #-}
    peek = error "CFMPQPoly.peek: Not defined"
    poke = error "CFMPQPoly.poke: Not defined"


foreign import ccall unsafe "fmpq_poly_init"
        fmpq_poly_init :: Ptr CFMPQPoly -> IO ()

foreign import ccall unsafe "fmpq_poly_init2"
        fmpq_poly_init2 :: Ptr CFMPQPoly -> CLong -> IO ()

foreign import ccall unsafe "fmpq_poly_realloc"
        fmpq_poly_realloc :: Ptr CFMPQPoly -> CLong -> IO ()

foreign import ccall unsafe "fmpq_poly_clear"
        fmpq_poly_clear :: Ptr CFMPQPoly -> IO ()

foreign import capi "flint/fmpq_poly.h value fmpq_poly_clear"
        p_fmpq_poly_clear :: FunPtr (Ptr CFMPQPoly -> IO ())


foreign import ccall unsafe "fmpq_poly_denref_wrapper"
        fmpq_poly_denref :: Ptr CFMPQPoly -> IO (Ptr CFMPZ)

foreign import capi unsafe "flint/fmpq_poly.h fmpq_poly_get_numerator"
        fmpq_poly_get_numerator :: Ptr CFMPZPoly -> Ptr CFMPQPoly -> IO ()


foreign import capi unsafe "flint/fmpq_poly.h fmpq_poly_degree"
        fmpq_poly_degree :: Ptr CFMPQPoly -> IO CLong

foreign import ccall unsafe "fmpq_poly_set_fmpq"
        fmpq_poly_set_fmpq :: Ptr CFMPQPoly -> Ptr CFMPQ -> IO ()

foreign import ccall unsafe "fmpq_poly_set_fmpz_poly"
        fmpq_poly_set_fmpz_poly :: Ptr CFMPQPoly -> Ptr CFMPZPoly -> IO ()

foreign import ccall unsafe "fmpq_poly_get_str"
        fmpq_poly_get_str :: Ptr CFMPQPoly -> IO CString

foreign import ccall unsafe "fmpq_poly_get_str_pretty"
        fmpq_poly_get_str_pretty :: Ptr CFMPQPoly -> CString -> IO CString


foreign import ccall unsafe "fmpq_poly_zero"
        fmpq_poly_zero :: Ptr CFMPQPoly -> IO ()

foreign import capi unsafe "flint/fmpq_poly.h fmpq_poly_one"
        fmpq_poly_one :: Ptr CFMPQPoly -> IO ()


foreign import ccall unsafe "fmpq_poly_neg"
        fmpq_poly_neg :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> IO ()       

foreign import ccall unsafe "fmpq_poly_inv"
        fmpq_poly_inv :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> IO ()       


foreign import capi unsafe "flint/fmpq_poly.h fmpq_poly_truncate"
        fmpq_poly_truncate :: Ptr CFMPQPoly -> CLong -> IO ()       
        

foreign import ccall unsafe "fmpq_poly_get_coeff_fmpq"
        fmpq_poly_get_coeff_fmpq :: Ptr CFMPQ -> Ptr CFMPQPoly -> CLong -> IO ()

foreign import ccall unsafe "fmpq_poly_set_coeff_fmpq"
        fmpq_poly_set_coeff_fmpq :: Ptr CFMPQPoly -> CLong -> Ptr CFMPQ -> IO ()

foreign import ccall unsafe "fmpq_poly_equal"
        fmpq_poly_equal :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> IO CInt

foreign import ccall unsafe "fmpq_poly_cmp"
        fmpq_poly_cmp :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> IO CInt

foreign import capi unsafe "flint/fmpq_poly.h fmpq_poly_is_zero"
        fmpq_poly_is_zero :: Ptr CFMPQPoly -> IO CInt

foreign import capi unsafe "flint/fmpq_poly.h fmpq_poly_is_one"
        fmpq_poly_is_one :: Ptr CFMPQPoly -> IO CInt


foreign import ccall unsafe "fmpq_poly_add"
        fmpq_poly_add :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> Ptr CFMPQPoly -> IO ()

foreign import ccall unsafe "fmpq_poly_sub"
        fmpq_poly_sub :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> Ptr CFMPQPoly -> IO ()

foreign import ccall unsafe "fmpq_poly_scalar_mul_fmpq"
        fmpq_poly_scalar_mul_fmpq :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> Ptr CFMPQ -> IO ()

foreign import ccall unsafe "fmpq_poly_scalar_div_fmpq"
        fmpq_poly_scalar_div_fmpq :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> Ptr CFMPQ -> IO ()

foreign import ccall unsafe "fmpq_poly_mul"
        fmpq_poly_mul :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> Ptr CFMPQPoly -> IO ()

        
foreign import ccall unsafe "fmpq_poly_shift_left"
        fmpq_poly_shift_left :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> CLong -> IO ()

foreign import ccall unsafe "fmpq_poly_shift_right"
        fmpq_poly_shift_right :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> CLong -> IO ()


foreign import ccall unsafe "fmpq_poly_divrem"
        fmpq_poly_divrem :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> Ptr CFMPQPoly -> Ptr CFMPQPoly -> IO ()

foreign import ccall unsafe "fmpq_poly_div"
        fmpq_poly_div :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> Ptr CFMPQPoly -> IO ()

foreign import ccall unsafe "fmpq_poly_rem"
        fmpq_poly_rem :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> Ptr CFMPQPoly -> IO ()


foreign import capi unsafe "flint/fmpq_poly.h fmpq_poly_inv_series"
        fmpq_poly_inv_series :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> CLong -> IO ()

foreign import ccall unsafe "fmpq_poly_div_series"
        fmpq_poly_div_series :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> Ptr CFMPQPoly -> CLong -> IO ()


foreign import ccall unsafe "fmpq_poly_gcd"
        fmpq_poly_gcd :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> Ptr CFMPQPoly -> IO ()

foreign import ccall unsafe "fmpq_poly_xgcd"
        fmpq_poly_xgcd :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> Ptr CFMPQPoly -> Ptr CFMPQPoly -> Ptr CFMPQPoly -> IO ()

foreign import ccall unsafe "fmpq_poly_lcm"
        fmpq_poly_lcm :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> Ptr CFMPQPoly -> IO ()


foreign import ccall unsafe "fmpq_poly_make_monic"
        fmpq_poly_make_monic :: Ptr CFMPQPoly -> Ptr CFMPQPoly -> IO ()
