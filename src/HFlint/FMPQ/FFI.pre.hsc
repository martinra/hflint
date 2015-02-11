{-# LANGUAGE
    ForeignFunctionInterface
  , CApiFFI
  , EmptyDataDecls
  , FlexibleInstances
  , TypeFamilies
  #-}

module HFlint.FMPQ.FFI
where

#include <flint/fmpq.h>

import Foreign.C.String ( CString )
import Foreign.C.Types ( CULong(..)
                       , CInt(..) )
import Foreign.ForeignPtr ( ForeignPtr )
import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.Storable ( Storable(..) )


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data CFMPQ
newtype FMPQ = FMPQ (ForeignPtr CFMPQ)
data CFMPQType
data FMPQType = FMPQType

instance Storable CFMPQ where
    sizeOf _ = #size fmpq
    alignment _ = #alignment fmpq
    peek = error "CFMPQ.peek: Not defined"
    poke = error "CFMPQ.poke: Not defined"


foreign import capi unsafe "flint/fmpq.h fmpq_init"
        fmpq_init :: Ptr CFMPQ -> IO ()

foreign import capi unsafe "flint/fmpq.h fmpq_clear"
        fmpq_clear :: Ptr CFMPQ -> IO ()

foreign import capi "flint/fmpq.h value fmpq_clear"
        p_fmpq_clear :: FunPtr (Ptr CFMPQ -> IO ())


foreign import ccall unsafe "fmpq_numref_wrapper"
        fmpq_numref :: Ptr CFMPQ -> IO (Ptr CFMPQ)

foreign import ccall unsafe "fmpq_denref_wrapper"
        fmpq_denref :: Ptr CFMPQ -> IO (Ptr CFMPQ)


foreign import capi unsafe "fmpq_set"
        fmpq_set :: Ptr CFMPQ -> Ptr CFMPQ -> IO ()

foreign import ccall unsafe "fmpq_set_fmpz_frac"
        fmpq_set_fmpz_frac :: Ptr CFMPQ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()


foreign import ccall unsafe "fmpq_get_str"
        fmpq_get_str :: CString -> CInt -> Ptr CFMPQ -> IO CString



foreign import capi "flint/fmpq.h fmpq_sgn"
        fmpq_sgn :: Ptr CFMPQ -> IO CInt

foreign import capi unsafe "flint/fmpq.h fmpq_neg"
        fmpq_neg :: Ptr CFMPQ -> Ptr CFMPQ -> IO ()

foreign import capi "flint/fmpq.h fmpq_abs"
        fmpq_abs :: Ptr CFMPQ -> Ptr CFMPQ -> IO ()


foreign import ccall unsafe "fmpq_add"
        fmpq_add :: Ptr CFMPQ -> Ptr CFMPQ -> Ptr CFMPQ -> IO ()

foreign import ccall unsafe "fmpq_sub"
        fmpq_sub :: Ptr CFMPQ -> Ptr CFMPQ -> Ptr CFMPQ -> IO ()

foreign import ccall unsafe "fmpq_mul"
        fmpq_mul :: Ptr CFMPQ -> Ptr CFMPQ -> Ptr CFMPQ -> IO ()

foreign import ccall unsafe "fmpq_div"
        fmpq_div :: Ptr CFMPQ -> Ptr CFMPQ -> Ptr CFMPQ -> IO ()

foreign import ccall unsafe "fmpq_inv"
        fmpq_inv :: Ptr CFMPQ -> Ptr CFMPQ -> IO ()
