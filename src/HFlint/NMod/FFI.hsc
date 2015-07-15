{-# LANGUAGE
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

#include <flint/fmpz.h>
#include <flint/ulong_extras.h>

import Control.Monad ( when )
import Data.Proxy
import Data.Reflection
import Data.Word ( Word64 )

import Foreign.C.Types ( CULong(..) )
import Foreign.ForeignPtr ( ForeignPtr
                          , mallocForeignPtr
                          , withForeignPtr )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable(..) )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.Internal.Context
import HFlint.Internal.FlintPrim


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

type FlintLimb = CULong

newtype NMod ctxProxy = NMod {unNMod :: FlintLimb}
--type CNMod ctx = CFlint (NMod ctx)

newtype NModCtx = NModCtx (ForeignPtr CNModCtx)
type CNModCtx = CFlintCtx NModCtx


instance FlintContext NModCtx
  where
  data CFlintCtx NModCtx
  data FlintContextData NModCtx = NModCtxData Word64

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
    sizeOf _ = #{size nmod_t}
    {-# INLINE alignment #-}
    alignment _ = #{alignment nmod_t}
    peek = error "CNModCtx.peek: Not defined"
    poke = error "CNModCtx.poke: Not defined"


instance FlintPrim NModCtx NMod
  where
  type CFlintPrim NMod = FlintLimb

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
        nmod_init :: Ptr CNModCtx -> FlintLimb -> IO ()

foreign import ccall unsafe "nmod_n_additional"
        nmod_n :: Ptr CNModCtx -> IO FlintLimb

foreign import ccall unsafe "nmod_add_wrapper"
        nmod_add :: FlintLimb -> FlintLimb -> Ptr CNModCtx -> IO FlintLimb

foreign import ccall unsafe "nmod_sub_wrapper"
        nmod_sub :: FlintLimb -> FlintLimb -> Ptr CNModCtx -> IO FlintLimb

foreign import ccall unsafe "nmod_neg_wrapper"
        nmod_neg :: FlintLimb -> Ptr CNModCtx -> IO FlintLimb

foreign import ccall unsafe "nmod_mul_wrapper"
        nmod_mul :: FlintLimb -> FlintLimb -> Ptr CNModCtx -> IO FlintLimb

foreign import ccall unsafe "nmod_inv_wrapper"
        nmod_inv :: FlintLimb -> Ptr CNModCtx -> IO FlintLimb

foreign import ccall unsafe "nmod_div_wrapper"
        nmod_div :: FlintLimb -> FlintLimb -> Ptr CNModCtx -> IO FlintLimb


foreign import capi unsafe "flint/ulong_extras.h n_preinvert_limb"
  n_preinvert_limb :: FlintLimb -> IO FlintLimb
