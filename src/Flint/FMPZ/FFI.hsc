{-# LANGUAGE
    ForeignFunctionInterface
  , CApiFFI
  , EmptyDataDecls
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

#include <flint/fmpz.h>

module Flint.FMPZ.FFI
where


import Foreign.C.String (CString)
import Foreign.C.Types (CULong(..), CInt(..))
import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr
                          , mallocForeignPtr, addForeignPtrFinalizer )
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.Storable (Storable(..))

import Flint.Internal.FlintCalls

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


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

foreign import capi unsafe "flint/fmpz.h fmpz_set_ui"
        fmpz_set_ui :: Ptr CFMPZ -> CULong -> IO ()


foreign import ccall unsafe "fmpz_get_str"
        fmpz_get_str :: CString -> CInt -> Ptr CFMPZ -> IO CString


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


data CFMPZ
newtype FMPZ = FMPZ (ForeignPtr CFMPZ)

instance Storable CFMPZ where
    sizeOf _ = #{size fmpz}
    alignment _ = #{alignment fmpz}
    peek = error "CFMPZ.peek: Not defined"
    poke = error "CFMPZ.poke: Not defined"

instance Flint FMPZ CFMPZ where
    newFlint = do
      a <- mallocForeignPtr
      withForeignPtr a fmpz_init
      addForeignPtrFinalizer p_fmpz_clear a
      return $ FMPZ a

    withFlint (FMPZ a) f = withForeignPtr a $ \a' ->
                           f a' >>= \r -> return (FMPZ a,r)
