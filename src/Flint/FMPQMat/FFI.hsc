{-# LANGUAGE
    ForeignFunctionInterface
  , CApiFFI
  , EmptyDataDecls
  , FlexibleInstances
  , TypeFamilies
  #-}

#include <flint/flint.h>
#include <flint/fmpq_mat.h>

module Flint.FMPQMat.FFI
where

import Foreign.C.Types (CLong(..))
import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr
                          , mallocForeignPtr, addForeignPtrFinalizer )
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.Storable (Storable(..))

import System.IO.Unsafe (unsafePerformIO)

import Flint.Internal.Flint
import Flint.Internal.FlintMat
import Flint.FMPQ.FFI
import Flint.FMPQ


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


foreign import ccall unsafe "fmpq_mat_init"
        fmpq_mat_init :: Ptr CFMPQMat -> CLong -> CLong -> IO ()

foreign import capi unsafe "flint/fmpq_mat.h fmpq_mat_clear"
        fmpq_mat_clear :: Ptr CFMPQMat -> IO ()

foreign import capi "flint/fmpq_mat.h value fmpq_mat_clear"
        p_fmpq_mat_clear :: FunPtr (Ptr CFMPQMat -> IO ())


foreign import ccall unsafe "fmpq_mat_nrows_wrapper"
        fmpq_mat_nrows :: Ptr CFMPQMat -> IO CLong

foreign import ccall unsafe "fmpq_mat_ncols_wrapper"
        fmpq_mat_ncols :: Ptr CFMPQMat -> IO CLong

foreign import ccall unsafe "fmpq_mat_entryref_wrapper"
        fmpq_mat_entryref :: Ptr CFMPQMat -> CLong -> CLong -> IO (Ptr CFMPQ)


foreign import ccall unsafe "fmpq_mat_add"
        fmpq_mat_add :: Ptr CFMPQMat -> Ptr CFMPQMat -> Ptr CFMPQMat -> IO ()

foreign import ccall unsafe "fmpq_mat_sub"
        fmpq_mat_sub :: Ptr CFMPQMat -> Ptr CFMPQMat -> Ptr CFMPQMat -> IO ()

foreign import ccall unsafe "fmpq_mat_mul"
        fmpq_mat_mul :: Ptr CFMPQMat -> Ptr CFMPQMat -> Ptr CFMPQMat -> IO ()


foreign import ccall unsafe "fmpq_mat_rref"
        fmpq_mat_rref :: Ptr CFMPQMat -> Ptr CFMPQMat -> IO ()

data CFMPQMat
newtype FMPQMat = FMPQMat (ForeignPtr CFMPQMat)
data CFMPQMatType
data FMPQMatType = FMPQMatType

instance Storable CFMPQMat where
    sizeOf _ = #size fmpq_mat_struct
    alignment _ = #alignment fmpq_mat_struct
    peek = error "CFMPQMat.peek: Not defined"
    poke = error "CFMPQMat.poke: Not defined"


instance Flint FMPQMat where
    type CFlint FMPQMat = CFMPQMat
    type FlintType FMPQMat = (FMPQType, Int, Int)
    type CFlintType FMPQMat = CFMPQMatType

    flintType m = (FMPQType, r, c)
        where
          r = fromIntegral $ unsafePerformIO $ lift0Flint (const fmpq_mat_nrows) m
          c = fromIntegral $ unsafePerformIO $ lift0Flint (const fmpq_mat_ncols) m

    newFlint (_,r,c) = do
      a <- mallocForeignPtr
      withForeignPtr a $ \aptr -> fmpq_mat_init aptr (fromIntegral r) (fromIntegral c)
      addForeignPtrFinalizer p_fmpq_mat_clear a
      return $ FMPQMat a

    withFlint (FMPQMat a) f = withForeignPtr a $ \a' ->
                              f undefined a' >>= \r -> return (FMPQMat a,r)

instance FlintMat FMPQMat where
    type FlintMatEntry FMPQMat = FMPQ

    flintMatEntry m i j = unsafePerformIO $
                          withNewFMPQ_ $ const $ \eptr ->
                          withFlint_ m $ const $ \mptr ->
                          fmpq_mat_entryref mptr (fromIntegral i) (fromIntegral j) >>=
                          fmpq_set eptr


