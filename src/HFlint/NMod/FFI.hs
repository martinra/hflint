{-# LINE 1 "FFI.pre.hsc" #-}
{-# LANGUAGE
{-# LINE 2 "FFI.pre.hsc" #-}
    CApiFFI
  , EmptyDataDecls
  , FlexibleContexts
  , FlexibleInstances
  , ForeignFunctionInterface
  , InstanceSigs
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TupleSections
  , TypeFamilies
  , UndecidableInstances
  #-}

module HFlint.NMod.FFI
where


{-# LINE 19 "FFI.pre.hsc" #-}

import Control.Monad ( when )
import Data.Proxy
import Data.Reflection

import Foreign.C.Types ( CULong(..) )
import Foreign.ForeignPtr ( ForeignPtr
                          , mallocForeignPtr
                          , withForeignPtr )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable(..) )
import Numeric.Natural
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.Internal.Context
import HFlint.Internal.FlintPrim



{-# LINE 38 "FFI.pre.hsc" #-}

newtype NMod ctxProxy = NMod {unNMod :: CULong}
--type CNMod ctx = CFlint (NMod ctx)

newtype NModCtx = NModCtx (ForeignPtr CNModCtx)
type CNModCtx = CFlintCtx NModCtx


instance FlintContext NModCtx
  where
  data CFlintCtx NModCtx
  -- this should actually be the intersection of Natural and Int
  data FlintContextData NModCtx = NModCtxData Natural

  {-# INLINE newFlintContext #-}
  newFlintContext (NModCtxData n) = do
    when (n<=1) $
      error "NModCtx.newFlintContext: Modulus must be at least 2"
    ctx <- mallocForeignPtr
    withForeignPtr ctx $ \ctxptr -> nmod_init ctxptr (fromIntegral n)
    return $ NModCtx ctx
  
  {-# INLINE withFlintContext #-}
  withFlintContext (NModCtx fptr) f = unsafePerformIO $
    withForeignPtr fptr $ \ptr -> return $ reify ptr f

instance Storable CNModCtx where
    {-# INLINE sizeOf #-}
    sizeOf _ = (24)
{-# LINE 67 "FFI.pre.hsc" #-}
    {-# INLINE alignment #-}
    alignment _ = 8
{-# LINE 69 "FFI.pre.hsc" #-}
    peek = error "CNModCtx.peek: Not defined"
    poke = error "CNModCtx.poke: Not defined"


instance FlintPrim NModCtx NMod
  where
  type CFlintPrim NMod = CULong

  {-# INLINE withFlintPrimCtx #-}
  withFlintPrimCtx
    :: forall ctxProxy b .
       ReifiesFlintContext NModCtx ctxProxy
    => NMod ctxProxy
    -> (CFlintPrim NMod -> Ptr (CFlintCtx NModCtx) -> IO b)
    -> IO b
  withFlintPrimCtx (NMod a) f = 
    f a $ reflect (Proxy :: Proxy ctxProxy)

  {-# INLINE withNewFlintPrimCtx #-}
  withNewFlintPrimCtx
    :: forall ctxProxy .
       ReifiesFlintContext NModCtx ctxProxy
    => (Ptr (CFlintCtx NModCtx) -> IO (CFlintPrim NMod))
    -> IO (NMod ctxProxy)
  withNewFlintPrimCtx f =
    NMod <$> f (reflect (Proxy :: Proxy ctxProxy))

  {-# INLINE withFlintPrim #-}
  withFlintPrim (NMod a) f = f a


foreign import capi unsafe "flint/nmod_vec.h nmod_init"
        nmod_init :: Ptr CNModCtx -> CULong -> IO ()

foreign import ccall unsafe "nmod_n_additional"
        nmod_n :: Ptr CNModCtx -> IO CULong

foreign import ccall unsafe "nmod_add_wrapper"
        nmod_add :: CULong -> CULong -> Ptr CNModCtx -> IO CULong

foreign import ccall unsafe "nmod_sub_wrapper"
        nmod_sub :: CULong -> CULong -> Ptr CNModCtx -> IO CULong

foreign import ccall unsafe "nmod_neg_wrapper"
        nmod_neg :: CULong -> Ptr CNModCtx -> IO CULong

foreign import ccall unsafe "nmod_mul_wrapper"
        nmod_mul :: CULong -> CULong -> Ptr CNModCtx -> IO CULong

foreign import ccall unsafe "nmod_inv_wrapper"
        nmod_inv :: CULong -> Ptr CNModCtx -> IO CULong

foreign import ccall unsafe "nmod_div_wrapper"
        nmod_div :: CULong -> CULong -> Ptr CNModCtx -> IO CULong
