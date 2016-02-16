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

import Foreign.C.Types ( CULong(..) )
import Foreign.Marshal.Alloc ( malloc, free )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable(..) )

import HFlint.Internal.Context
import HFlint.Internal.FlintPrim


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

type FlintLimb = CULong

data NMod ctxProxy = NMod {unNMod :: {-# UNPACK #-} !FlintLimb}
--type CNMod ctx = CFlint (NMod ctx)

newtype NModCtx = NModCtx (Ptr CNModCtx)
type CNModCtx = CFlintContext NModCtx


instance FlintContext NModCtx
  where
  data CFlintContext NModCtx
  data FlintContextData NModCtx = NModCtxData FlintLimb

  {-# INLINE newFlintContext #-}
  newFlintContext (NModCtxData n) = do
    when (n<=1) $
      error "NModCtx.newFlintContext: Modulus must be at least 2"
    ctx <- malloc
    nmod_init ctx (fromIntegral n)
    return $ NModCtx ctx

  {-# INLINE freeFlintContext #-}
  freeFlintContext (NModCtx ctx) = free ctx

  {-# INLINE withFlintContext #-}
  withFlintContext (NModCtx ctx) f = reify ctx f

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
    -> (CFlintPrim NMod -> Ptr (CFlintContext NModCtx) -> IO b)
    -> IO b
  withFlintPrimCtx (NMod a) f = 
    f a $ reflect (Proxy :: Proxy ctxProxy)

  {-# INLINE withNewFlintPrimCtx #-}
  withNewFlintPrimCtx
    :: forall ctxProxy .
       ReifiesFlintContext NModCtx ctxProxy
    => (Ptr (CFlintContext NModCtx) -> IO (CFlintPrim NMod))
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
