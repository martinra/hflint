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

module HFlint.FMPQ.FFI
where


{-# LINE 15 "FFI.pre.hsc" #-}

import Foreign.C.String ( CString )
import Foreign.C.Types ( CInt(..) )
import Foreign.ForeignPtr ( ForeignPtr
                          , addForeignPtrFinalizer
                          , mallocForeignPtr
                          , withForeignPtr )
import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.Storable ( Storable(..) )

import HFlint.FMPZ.FFI
import HFlint.Internal.Flint



{-# LINE 30 "FFI.pre.hsc" #-}


newtype FMPQ = FMPQ (ForeignPtr CFMPQ)
type CFMPQ = CFlint FMPQ

instance Flint FMPQ where
  data CFlint FMPQ

  {-# INLINE newFlint #-}
  newFlint = do
    a <- mallocForeignPtr
    withForeignPtr a fmpq_init
    addForeignPtrFinalizer p_fmpq_clear a
    return $ FMPQ a

  {-# INLINE withFlint #-}
  withFlint (FMPQ a) f =
    withForeignPtr a $ \aptr ->
    f aptr >>= return . (FMPQ a,)


{-# INLINE withFMPQ #-}
withFMPQ :: FMPQ -> (Ptr CFMPQ -> IO b) -> IO (FMPQ, b)
withFMPQ = withFlint

{-# INLINE withFMPQ_ #-}
withFMPQ_ :: FMPQ -> (Ptr CFMPQ -> IO b) -> IO FMPQ
withFMPQ_ = withFlint_

{-# INLINE withNewFMPQ #-}
withNewFMPQ :: (Ptr CFMPQ -> IO b) -> IO (FMPQ, b)
withNewFMPQ = withNewFlint

{-# INLINE withNewFMPQ_ #-}
withNewFMPQ_ :: (Ptr CFMPQ -> IO b) -> IO FMPQ
withNewFMPQ_ = withNewFlint_


instance Storable CFMPQ where
  {-# INLINE sizeOf #-}
  sizeOf _ = (16)
{-# LINE 71 "FFI.pre.hsc" #-}
  {-# INLINE alignment #-}
  alignment _ = 8
{-# LINE 73 "FFI.pre.hsc" #-}
  peek = error "CFMPQ.peek: Not defined"
  poke = error "CFMPQ.poke: Not defined"


foreign import capi unsafe "flint/fmpq.h fmpq_init"
        fmpq_init :: Ptr CFMPQ -> IO ()

foreign import capi unsafe "flint/fmpq.h fmpq_clear"
        fmpq_clear :: Ptr CFMPQ -> IO ()

foreign import capi "flint/fmpq.h value fmpq_clear"
        p_fmpq_clear :: FunPtr (Ptr CFMPQ -> IO ())


foreign import ccall unsafe "fmpq_numref_wrapper"
        fmpq_numref :: Ptr CFMPQ -> IO (Ptr CFMPZ)

foreign import ccall unsafe "fmpq_denref_wrapper"
        fmpq_denref :: Ptr CFMPQ -> IO (Ptr CFMPZ)


foreign import capi unsafe "fmpq_set"
        fmpq_set :: Ptr CFMPQ -> Ptr CFMPQ -> IO ()

foreign import ccall unsafe "fmpq_set_fmpz_frac"
        fmpq_set_fmpz_frac :: Ptr CFMPQ -> Ptr CFMPZ -> Ptr CFMPZ -> IO ()


foreign import ccall unsafe "fmpq_get_str"
        fmpq_get_str :: CString -> CInt -> Ptr CFMPQ -> IO CString


foreign import capi unsafe "fmpq_zero"
        fmpq_zero :: Ptr CFMPQ -> IO ()

foreign import capi unsafe "fmpq_one"
        fmpq_one :: Ptr CFMPQ -> IO ()


foreign import capi unsafe "fmpq_is_zero"
        fmpq_is_zero :: Ptr CFMPQ -> IO CInt

foreign import capi unsafe "fmpq_equal"
        fmpq_equal :: Ptr CFMPQ -> Ptr CFMPQ -> IO CInt

foreign import ccall unsafe "fmpq_cmp"
        fmpq_cmp :: Ptr CFMPQ -> Ptr CFMPQ -> IO CInt


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
