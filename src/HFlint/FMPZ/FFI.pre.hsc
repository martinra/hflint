{-# LANGUAGE
    ForeignFunctionInterface
  , CApiFFI
  , EmptyDataDecls
  , FlexibleInstances
  , TypeFamilies
  #-}

module HFlint.FMPZ.FFI
where


#include <flint/fmpz.h>

import Foreign.C.String ( CString )
import Foreign.C.Types ( CULong(..)
                       , CInt(..) )
import Foreign.ForeignPtr ( ForeignPtr )
import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.Storable ( Storable(..) )


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data CFMPZ
newtype FMPZ = FMPZ (ForeignPtr CFMPZ)
data CFMPZType
data FMPZType = FMPZType

instance Storable CFMPZ where
    sizeOf _ = #{size fmpz}
    alignment _ = #{alignment fmpz}
    peek = error "CFMPZ.peek: Not defined"
    poke = error "CFMPZ.poke: Not defined"


foreign import capi unsafe "flint/fmpz.h fmpz_init"
        fmpz_init :: Ptr CFMPZ -> IO ()

foreign import capi unsafe "flint/fmpz.h fmpz_clear"
        fmpz_clear :: Ptr CFMPZ -> IO ()

foreign import capi "flint/fmpz.h value fmpz_clear"
        p_fmpz_clear :: FunPtr (Ptr CFMPZ -> IO ())


foreign import capi unsafe "flint/fmpz.h fmpz_zero"
        fmpz_zero :: Ptr CFMPZ -> IO ()

foreign import capi unsafe "flint/fmpz.h fmpz_one"
        fmpz_one :: Ptr CFMPZ -> IO ()

foreign import capi unsafe "flint/fmpz.h fmpz_is_zero"
        fmpz_is_zero :: Ptr CFMPZ -> IO CInt

foreign import capi unsafe "flint/fmpz.h fmpz_is_one"
        fmpz_is_one :: Ptr CFMPZ -> IO CInt

foreign import capi unsafe "flint/fmpz.h fmpz_is_pm1"
        fmpz_is_pm1 :: Ptr CFMPZ -> IO CInt


foreign import ccall unsafe "fmpz_set"
        fmpz_set :: Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import capi unsafe "flint/fmpz.h fmpz_set_ui"
        fmpz_set_ui :: Ptr CFMPZ -> CULong -> IO ()


foreign import ccall unsafe "fmpz_get_str"
        fmpz_get_str :: CString -> CInt -> Ptr CFMPZ -> IO CString


foreign import ccall unsafe "fmpz_equal"
	fmpz_equal :: Ptr CFMPZ -> Ptr CFMPZ -> IO CInt

foreign import ccall unsafe "fmpz_cmp"
	fmpz_cmp :: Ptr CFMPZ -> Ptr CFMPZ -> IO CInt


foreign import ccall unsafe "fmpz_sgn"
        fmpz_sgn :: Ptr CFMPZ -> IO CInt

foreign import capi unsafe "flint/fmpz.h fmpz_neg"
        fmpz_neg :: Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall unsafe "fmpz_abs"
        fmpz_abs :: Ptr CFMPZ -> Ptr CFMPZ -> IO ()


foreign import ccall unsafe "fmpz_add"
        fmpz_add :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall unsafe "fmpz_add_ui"
        fmpz_add_ui :: Ptr CFMPZ -> Ptr CFMPZ -> CULong -> IO ()

foreign import ccall unsafe "fmpz_sub"
        fmpz_sub :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall unsafe "fmpz_mul"
        fmpz_mul :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall unsafe "fmpz_mul_ui"
        fmpz_mul_ui :: Ptr CFMPZ -> Ptr CFMPZ -> CULong -> IO ()

foreign import ccall unsafe "fmpz_fdiv_q"
        fmpz_fdiv_q :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall unsafe "fmpz_fdiv_r"
        fmpz_fdiv_r :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall unsafe "fmpz_fdiv_qr"
        fmpz_fdiv_qr :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall unsafe "fmpz_tdiv_q"
        fmpz_tdiv_q :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall unsafe "fmpz_tdiv_qr"
        fmpz_tdiv_qr :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall unsafe "fmpz_fdiv_ui"
        fmpz_fdiv_ui :: Ptr CFMPZ -> CULong -> IO CULong

foreign import ccall unsafe "fmpz_fdiv_q_ui"
        fmpz_fdiv_q_ui :: Ptr CFMPZ -> Ptr CFMPZ -> CULong -> IO ()


foreign import ccall unsafe "fmpz_gcd"
        fmpz_gcd :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall unsafe "fmpz_xgcd"
        fmpz_xgcd :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()
