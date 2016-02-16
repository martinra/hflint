{-# LANGUAGE
    CApiFFI
  , EmptyDataDecls
  , FlexibleInstances
  , ForeignFunctionInterface
  , MultiParamTypeClasses
  , TupleSections
  , TypeFamilies
  #-}
{-# CFILES csrc/flint_definie_wrappers.c #-}

module HFlint.FMPQMat.FFI
where

#include <flint/fmpq_mat.h>

import Foreign.C.Types ( CLong(..) )
import Foreign.ForeignPtr ( ForeignPtr
                          , addForeignPtrFinalizer
                          , mallocForeignPtr
                          , withForeignPtr )
import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.Storable ( Storable(..) )

import HFlint.FMPQ.FFI
import HFlint.Internal.Flint


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


data FMPQMat = FMPQMat {-# UNPACK #-} !(ForeignPtr CFMPQMat)
type CFMPQMat = CFlint FMPQMat

{-# INLINE newFMPQMat #-}
newFMPQMat :: CLong -> CLong ->  IO FMPQMat
newFMPQMat r c = do
  a <- mallocForeignPtr
  withForeignPtr a $ \aptr -> fmpq_mat_init aptr r c
  addForeignPtrFinalizer p_fmpq_mat_clear a
  return $ FMPQMat a

{-# INLINE withFMPQMat #-}
withFMPQMat :: FMPQMat -> (Ptr CFMPQMat -> IO b) -> IO (FMPQMat, b)
withFMPQMat (FMPQMat a) f =
  withForeignPtr a $ \aptr ->
  f aptr >>= return . (FMPQMat a,)

{-# INLINE withFMPQMat_ #-}
withFMPQMat_ :: FMPQMat -> (Ptr CFMPQMat -> IO b) -> IO FMPQMat
withFMPQMat_ a f = fst <$> withFMPQMat a f 

{-# INLINE withNewFMPQMat #-}
withNewFMPQMat :: Int -> Int -> (Ptr CFMPQMat -> IO b) -> IO (FMPQMat, b)
withNewFMPQMat r c f = do
  let r' = fromIntegral r
  let c' = fromIntegral c
  mat <- newFMPQMat r' c'
  withFMPQMat mat f

{-# INLINE withNewFMPQMat_ #-}
withNewFMPQMat_ :: Int -> Int -> (Ptr CFMPQMat -> IO b) -> IO FMPQMat
withNewFMPQMat_ r c f = fst <$> withNewFMPQMat r c f 


instance Storable CFMPQMat where
  {-# INLINE sizeOf #-}
  sizeOf _ = #size fmpq_mat_struct
  {-# INLINE alignment #-}
  alignment _ = #alignment fmpq_mat_struct
  peek = error "CFMPQMat.peek: Not defined"
  poke = error "CFMPQMat.poke: Not defined"


foreign import capi unsafe "flint/fmpq_mat.h fmpq_mat_init"
        fmpq_mat_init :: Ptr CFMPQMat -> CLong -> CLong -> IO ()

foreign import capi unsafe "flint/fmpq_mat.h fmpq_mat_clear"
        fmpq_mat_clear :: Ptr CFMPQMat -> IO ()

foreign import capi "flint/fmpq_mat.h value fmpq_mat_clear"
        p_fmpq_mat_clear :: FunPtr (Ptr CFMPQMat -> IO ())


foreign import ccall unsafe "static flint_define_wrappers.h fmpq_mat_entry_wrapper"
        fmpq_mat_entry :: Ptr CFMPQMat -> CLong -> CLong -> IO (Ptr CFMPQ)

foreign import ccall unsafe "static flint_define_wrappers.h fmpq_mat_nrows_wrapper"
        fmpq_mat_nrows :: Ptr CFMPQMat -> IO CLong

foreign import ccall unsafe "static flint_define_wrappers.h fmpq_mat_ncols_wrapper"
        fmpq_mat_ncols :: Ptr CFMPQMat -> IO CLong


foreign import ccall unsafe "static flint_define_wrappers.h fmpq_mat_rref"
        fmpq_mat_rref :: Ptr CFMPQMat -> Ptr CFMPQMat -> IO CLong
