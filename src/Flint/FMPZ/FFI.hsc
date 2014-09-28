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


import Foreign.C.String(CString)
import Foreign.C.Types(CLong(..), CInt(..))
import Foreign.ForeignPtr( ForeignPtr, withForeignPtr
                         , mallocForeignPtr, addForeignPtrFinalizer )
import Foreign.Ptr(Ptr, FunPtr)
import Foreign.Storable(Storable(..))

import Data.Int
import Flint.Internal.FlintCalls


foreign import capi unsafe "flint/fmpz.h fmpz_init"
        fmpz_init :: Ptr CFMPZ -> IO ()

foreign import capi unsafe "flint/fmpz.h fmpz_zero"
        fmpz_zero :: Ptr CFMPZ -> IO ()

foreign import capi unsafe "flint/fmpz.h fmpz_set_si"
        fmpz_set_si :: Ptr CFMPZ -> CLong -> IO ()

foreign import capi unsafe "flint/fmpz.h fmpz_clear"
        fmpz_clear :: Ptr CFMPZ -> IO ()

foreign import capi "flint/fmpz.h value fmpz_clear"
        p_fmpz_clear :: FunPtr (Ptr CFMPZ -> IO ())

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

-- NOT IMPLEMENTED IN FLINT 2.4
-- foreign import ccall unsafe "fmpz_add_si"
--        fmpz_add_si :: Ptr CFMPZ -> Ptr CFMPZ -> CLong -> IO ()

foreign import ccall unsafe "fmpz_sub"
        fmpz_sub :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall unsafe "fmpz_mul"
        fmpz_mul :: Ptr CFMPZ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()

foreign import ccall unsafe "fmpz_mul_si"
        fmpz_mul_si :: Ptr CFMPZ -> Ptr CFMPZ -> CLong -> IO ()


data CFMPZ
newtype FMPZ = FMPZ (ForeignPtr CFMPZ)

instance Storable CFMPZ where
    sizeOf _ = #size fmpz
    alignment _ = alignment (undefined :: #{type fmpz})
    peek = error "CFMPZ.peek: Not defined"
    poke = error "CFMPZ.poke: Not defined"

instance Flint FMPZ CFMPZ where
    newFlint = do
      a <- mallocForeignPtr
      withForeignPtr a fmpz_init
      addForeignPtrFinalizer p_fmpz_clear a
      return $ FMPZ a

    withFlint f (FMPZ a) = withForeignPtr a f'
        where
          f' a' = do
            r <- f a'
            return (FMPZ a,r)
