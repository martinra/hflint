{-#   LANGUAGE ForeignFunctionInterface
    , CApiFFI #-}

#include <flint/fmpz.h>

module Flint.FMPZ.FFI
where

import Foreign.C.String(CString)
import Foreign.C.Types(CLong(..), CInt(..))
import Foreign.Ptr(Ptr, FunPtr)
import Foreign.ForeignPtr( ForeignPtr, withForeignPtr
                         , mallocForeignPtr, addForeignPtrFinalizer )
import Control.Applicative((<$>))
import System.IO.Unsafe (unsafePerformIO)


foreign import capi unsafe "flint/fmpz.h fmpz_init"
        fmpz_init :: Ptr CFMPZ -> IO ()

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

type CFMPZ = CLong
newtype FMPZ = FMPZ (ForeignPtr CFMPZ)

withFMPZ :: FMPZ -> (Ptr CFMPZ -> IO b) -> IO b
withFMPZ (FMPZ a) = withForeignPtr a

newFMPZ :: IO FMPZ
newFMPZ = do
  a <- mallocForeignPtr
  addForeignPtrFinalizer p_fmpz_clear a
  return $ FMPZ a

withNewFMPZ :: (Ptr CFMPZ -> IO a) -> (FMPZ, IO a)
withNewFMPZ f = (unsafePerformIO $ fst <$> ab, snd <$> ab)
    where
      ab = do
        a <- newFMPZ
        b <- withFMPZ a f
        return (a,b)

withNewFMPZ_ :: (Ptr CFMPZ -> IO a) -> FMPZ
withNewFMPZ_ = fst . withNewFMPZ
