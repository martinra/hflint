{-# LANGUAGE
    CApiFFI
  , EmptyDataDecls
  , FlexibleInstances
  , ForeignFunctionInterface
  , MultiParamTypeClasses
  , TupleSections
  , TypeFamilies
  #-}

module HFlint.FMPZ.FFI
where

#include <flint/fmpz.h>

import Foreign.C.String ( CString )
import Foreign.C.Types ( CULong(..)
                       , CInt(..) )
import Foreign.ForeignPtr ( ForeignPtr
                          , addForeignPtrFinalizer
                          , mallocForeignPtr
                          , withForeignPtr )
import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.Storable ( Storable(..) )

import HFlint.Internal.Flint
import HFlint.NMod.FFI


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


data FMPZ = FMPZ {-# UNPACK #-} !(ForeignPtr CFMPZ)
type CFMPZ = CFlint FMPZ

instance Flint FMPZ where
  data CFlint FMPZ

  {-# INLINE newFlint #-}
  newFlint = do
    a <- mallocForeignPtr
    withForeignPtr a fmpz_init
    addForeignPtrFinalizer p_fmpz_clear a
    return $ FMPZ a

  {-# INLINE withFlint #-}
  withFlint (FMPZ a) f = 
    withForeignPtr a $ \aptr ->
    f aptr >>= return . (FMPZ a,)


{-# INLINE withFMPZ #-}
withFMPZ :: FMPZ -> (Ptr CFMPZ -> IO b) -> IO (FMPZ, b)
withFMPZ = withFlint 

{-# INLINE withFMPZ_ #-}
withFMPZ_ :: FMPZ -> (Ptr CFMPZ -> IO b) -> IO FMPZ
withFMPZ_ = withFlint_

{-# INLINE withNewFMPZ #-}
withNewFMPZ :: (Ptr CFMPZ -> IO b) -> IO (FMPZ, b)
withNewFMPZ = withNewFlint

{-# INLINE withNewFMPZ_ #-}
withNewFMPZ_ :: (Ptr CFMPZ -> IO b) -> IO FMPZ
withNewFMPZ_ = withNewFlint_


instance Storable CFMPZ where
    {-# INLINE sizeOf #-}
    sizeOf _ = #{size fmpz}
    {-# INLINE alignment #-}
    alignment _ = #{alignment fmpz}
    peek = error "CFMPZ.peek: Not defined"
    poke = error "CFMPZ.poke: Not defined"


foreign import capi  "flint/fmpz.h fmpz_init"
        fmpz_init :: Ptr CFMPZ -> IO ()

foreign import capi  "flint/fmpz.h fmpz_clear"
        fmpz_clear :: Ptr CFMPZ -> IO ()

foreign import capi "flint/fmpz.h value fmpz_clear"
        p_fmpz_clear :: FunPtr (Ptr CFMPZ -> IO ())


foreign import ccall "fmpz_size"
  fmpz_size :: Ptr CFMPZ -> IO CInt


foreign import capi  "flint/fmpz.h fmpz_zero"
        fmpz_zero :: Ptr CFMPZ -> IO ()

foreign import capi  "flint/fmpz.h fmpz_one"
        fmpz_one :: Ptr CFMPZ -> IO ()

foreign import capi  "flint/fmpz.h fmpz_is_zero"
        fmpz_is_zero :: Ptr CFMPZ -> IO CInt

foreign import capi  "flint/fmpz.h fmpz_is_one"
        fmpz_is_one :: Ptr CFMPZ -> IO CInt

foreign import capi  "flint/fmpz.h fmpz_is_pm1"
        fmpz_is_pm1 :: Ptr CFMPZ -> IO CInt


foreign import ccall  "fmpz_set"
        fmpz_set :: Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import capi  "flint/fmpz.h fmpz_set_ui"
        fmpz_set_ui :: Ptr CFMPZ -> CULong -> IO ()


foreign import ccall  "fmpz_get_str"
        fmpz_get_str :: CString -> CInt -> Ptr CFMPZ -> IO CString


foreign import ccall  "fmpz_equal"
        fmpz_equal :: Ptr CFMPZ -> Ptr CFMPZ -> IO CInt

foreign import ccall  "fmpz_cmp"
        fmpz_cmp :: Ptr CFMPZ -> Ptr CFMPZ -> IO CInt


foreign import ccall  "fmpz_sgn"
        fmpz_sgn :: Ptr CFMPZ -> IO CInt

foreign import capi  "flint/fmpz.h fmpz_neg"
        fmpz_neg :: Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall  "fmpz_abs"
        fmpz_abs :: Ptr CFMPZ -> Ptr CFMPZ -> IO ()


foreign import ccall  "fmpz_add"
        fmpz_add :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall  "fmpz_add_ui"
        fmpz_add_ui :: Ptr CFMPZ -> Ptr CFMPZ -> CULong -> IO ()

foreign import ccall  "fmpz_sub"
        fmpz_sub :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall  "fmpz_mul"
        fmpz_mul :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall  "fmpz_mul_ui"
        fmpz_mul_ui :: Ptr CFMPZ -> Ptr CFMPZ -> CULong -> IO ()

foreign import ccall  "fmpz_submul"
        fmpz_submul :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall  "fmpz_fdiv_q"
        fmpz_fdiv_q :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall  "fmpz_fdiv_r"
        fmpz_fdiv_r :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall  "fmpz_fdiv_qr"
        fmpz_fdiv_qr :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall  "fmpz_tdiv_q"
        fmpz_tdiv_q :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall  "fmpz_tdiv_qr"
        fmpz_tdiv_qr :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall  "fmpz_fdiv_ui"
        fmpz_fdiv_ui :: Ptr CFMPZ -> CULong -> IO CULong

foreign import ccall  "fmpz_fdiv_q_ui"
        fmpz_fdiv_q_ui :: Ptr CFMPZ -> Ptr CFMPZ -> CULong -> IO ()

foreign import ccall  "fmpz_divexact"
        fmpz_divexact :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()


foreign import ccall  "fmpz_gcd"
        fmpz_gcd :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall  "fmpz_xgcd"
        fmpz_xgcd :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall  "fmpz_lcm"
        fmpz_lcm :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall  "fmpz_CRT_ui"
  fmpz_CRT_ui :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> CULong -> CULong -> CInt -> IO ()

foreign import ccall  "_fmpz_CRT_ui_precomp"
  fmpz_CRT_ui_precomp :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> FlintLimb -> FlintLimb -> FlintLimb -> Ptr CFMPZ -> FlintLimb -> CInt -> IO ()
