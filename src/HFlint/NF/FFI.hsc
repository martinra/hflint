{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HFlint.NF.FFI
where

#include <flint/nf.h>
#include <flint/nf_elem.h>

import Control.Monad ( void )
import Data.Proxy
import Data.Reflection
import Foreign.C.String ( CString )
import Foreign.C.Types ( CLong(..)
                       , CULong(..)
                       , CInt(..) )
import Foreign.ForeignPtr ( ForeignPtr
                          , addForeignPtrFinalizerEnv
                          , mallocForeignPtr
                          , withForeignPtr )
import Foreign.Marshal.Alloc ( malloc, free )
import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.Storable ( Storable(..) )

import HFlint.Internal.Context
import HFlint.Internal.Flint
import HFlint.Internal.FlintWithContext
import HFlint.FMPQ.FFI
import HFlint.FMPQPoly.FFI
import HFlint.FMPZ.FFI


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


newtype NFCtx = NFCtx (Ptr CNFCtx)
type CNFCtx = CFlintContext NFCtx

data NF ctxProxy = NF {-# UNPACK #-} !(ForeignPtr CNF)
type CNF = CFlintCtx NF


instance FlintContext NFCtx
  where
  data CFlintContext NFCtx
  data FlintContextData NFCtx = NFCtxData FMPQPoly

  {-# INLINE newFlintContext #-}
  newFlintContext (NFCtxData p) = do
    ctx <- malloc
    void $ withFlint_ p $ \pptr -> nf_init ctx pptr
    return $ NFCtx ctx

  {-# INLINE freeFlintContext #-}
  freeFlintContext (NFCtx ctx) = do
    nf_clear ctx
    free ctx

  {-# INLINE withFlintContext #-}
  withFlintContext (NFCtx ctx) f = reify ctx f

instance Storable CNFCtx where
    {-# INLINE sizeOf #-}
    sizeOf _ = #{size nf_t}
    {-# INLINE alignment #-}
    alignment _ = #{alignment nf_t}
    peek = error "CNFCtx.peek: Not defined"
    poke = error "CNFCtx.poke: Not defined"


instance FlintWithContext NFCtx NF where
  data CFlintCtx NF

  {-# INLINE newFlintCtx #-}
  newFlintCtx
    :: forall ctxProxy
    .  ReifiesFlintContext NFCtx ctxProxy
    => IO (NF ctxProxy)
  newFlintCtx = do
    let ctxptr = reflect (Proxy :: Proxy ctxProxy)
    a <- mallocForeignPtr
    withForeignPtr a $ \aptr ->
      nf_elem_init aptr ctxptr
    addForeignPtrFinalizerEnv p_nf_elem_clear_wrapper ctxptr a
    return $ (NF a :: NF ctxProxy)

  {-# INLINE withFlintCtx #-}
  withFlintCtx (NF a :: NF ctxProxy) f =
    let ctxptr = reflect (Proxy :: Proxy ctxProxy)
    in withForeignPtr a $ \aptr ->
       f aptr ctxptr >>= return . (NF a,)

{-# INLINE withNF #-}
withNF
  :: ReifiesFlintContext NFCtx ctxProxy
  => NF ctxProxy -> (Ptr CNF -> Ptr CNFCtx -> IO b) -> IO (NF ctxProxy, b)
withNF = withFlintCtx 

{-# INLINE withNF_ #-}
withNF_
  :: ReifiesFlintContext NFCtx ctxProxy
  => NF ctxProxy -> (Ptr CNF -> Ptr CNFCtx -> IO b) -> IO (NF ctxProxy)
withNF_ = withFlintCtx_

{-# INLINE withNewNF #-}
withNewNF
  :: ReifiesFlintContext NFCtx ctxProxy
  => (Ptr CNF -> Ptr CNFCtx -> IO b) -> IO (NF ctxProxy, b)
withNewNF = withNewFlintCtx

{-# INLINE withNewNF_ #-}
withNewNF_
  :: ReifiesFlintContext NFCtx ctxProxy
  => (Ptr CNF -> Ptr CNFCtx -> IO b) -> IO (NF ctxProxy)
withNewNF_ = withNewFlintCtx_


instance Storable CNF where
    {-# INLINE sizeOf #-}
    sizeOf _ = #{size nf_elem_t}
    {-# INLINE alignment #-}
    alignment _ = #{alignment nf_elem_t}
    peek = error "CNF.peek: Not defined"
    poke = error "CNF.poke: Not defined"


foreign import ccall unsafe "flint/nf.h nf_init"
        nf_init :: Ptr CNFCtx -> Ptr CFMPQPoly -> IO ()

foreign import ccall unsafe "flint/nf.h nf_clear"
        nf_clear :: Ptr CNFCtx -> IO ()

foreign import capi unsafe "flint/nf.h nf_degree"
        nf_degree :: Ptr CNFCtx -> IO CLong


foreign import ccall unsafe "flint/nf_elem.h nf_elem_init"
        nf_elem_init :: Ptr CNF -> Ptr CNFCtx -> IO ()

foreign import ccall unsafe "flint/nf_elem.h nf_elem_clear"
        nf_elem_clear :: Ptr CNF -> Ptr CNFCtx -> IO ()

foreign import capi "flint_define_wrappers.h value nf_elem_clear_wrapper"
        p_nf_elem_clear_wrapper :: FunPtr (Ptr CNFCtx -> Ptr CNF -> IO ())


foreign import ccall unsafe "flint/nf_elem.h nf_elem_equal"
        nf_elem_equal :: Ptr CNF -> Ptr CNF -> Ptr CNFCtx -> IO CInt

foreign import ccall unsafe "flint/nf_elem.h nf_elem_is_zero"
        nf_elem_is_zero :: Ptr CNF -> Ptr CNFCtx -> IO CInt

foreign import ccall unsafe "flint/nf_elem.h nf_elem_is_one"
        nf_elem_is_one :: Ptr CNF -> Ptr CNFCtx -> IO CInt


foreign import ccall unsafe "flint/nf_elem.h nf_elem_get_str_pretty"
        nf_elem_get_str_pretty :: Ptr CNF -> CString -> Ptr CNFCtx -> IO CString


foreign import ccall unsafe "flint/nf_elem.h nf_elem_zero"
        nf_elem_zero :: Ptr CNF -> Ptr CNFCtx -> IO ()

foreign import ccall unsafe "flint/nf_elem.h nf_elem_one"
        nf_elem_one :: Ptr CNF -> Ptr CNFCtx -> IO ()

foreign import ccall unsafe "flint/nf_elem.h nf_elem_gen"
        nf_elem_gen :: Ptr CNF -> Ptr CNFCtx -> IO ()

foreign import ccall unsafe "flint/nf_elem.h nf_elem_set_fmpz"
        nf_elem_set_fmpz :: Ptr CNF -> Ptr CFMPZ -> Ptr CNFCtx -> IO ()

foreign import ccall unsafe "flint/nf_elem.h nf_elem_set_fmpq"
        nf_elem_set_fmpq :: Ptr CNF -> Ptr CFMPQ -> Ptr CNFCtx -> IO ()

foreign import ccall unsafe "flint/nf_elem.h nf_elem_set_fmpq_poly"
        nf_elem_set_fmpq_poly :: Ptr CNF -> Ptr CFMPQPoly -> Ptr CNFCtx -> IO ()


foreign import ccall unsafe "flint/nf_elem.h nf_elem_get_fmpq_poly"
        nf_elem_get_fmpq_poly :: Ptr CFMPQPoly -> Ptr CNF -> Ptr CNFCtx -> IO ()

foreign import ccall unsafe "flint/nf_elem.h nf_elem_get_coeff_fmpq"
        nf_elem_get_coeff_fmpq :: Ptr CFMPQ -> Ptr CNF -> CLong -> Ptr CNFCtx -> IO ()

foreign import ccall unsafe "flint/nf_elem.h nf_elem_get_coeff_fmpz"
        nf_elem_get_coeff_fmpz :: Ptr CFMPZ -> Ptr CNF -> CLong -> Ptr CNFCtx -> IO ()

foreign import ccall unsafe "flint/nf_elem.h nf_elem_get_den"
        nf_elem_get_den :: Ptr CFMPZ -> Ptr CNF -> Ptr CNFCtx -> IO ()

foreign import ccall unsafe "flint/nf_elem.h nf_elem_set"
        nf_elem_set :: Ptr CNF -> Ptr CNF -> Ptr CNFCtx -> IO ()


foreign import ccall unsafe "flint/nf_elem.h nf_elem_neg"
        nf_elem_neg :: Ptr CNF -> Ptr CNF -> Ptr CNFCtx -> IO ()


foreign import ccall unsafe "flint/nf_elem.h nf_elem_add_fmpq"
        nf_elem_add_fmpq :: Ptr CNF -> Ptr CNF -> Ptr CFMPQ -> Ptr CNFCtx -> IO ()

foreign import ccall unsafe "flint/nf_elem.h nf_elem_scalar_mul_fmpq"
        nf_elem_scalar_mul_fmpq :: Ptr CNF -> Ptr CNF -> Ptr CFMPQ -> Ptr CNFCtx -> IO ()

foreign import ccall unsafe "flint/nf_elem.h nf_elem_scalar_div_fmpz"
        nf_elem_scalar_div_fmpz :: Ptr CNF -> Ptr CNF -> Ptr CFMPZ -> Ptr CNFCtx -> IO ()

foreign import ccall unsafe "flint/nf_elem.h nf_elem_scalar_div_fmpq"
        nf_elem_scalar_div_fmpq :: Ptr CNF -> Ptr CNF -> Ptr CFMPQ -> Ptr CNFCtx -> IO ()


foreign import ccall unsafe "flint/nf_elem.h nf_elem_add"
        nf_elem_add :: Ptr CNF -> Ptr CNF -> Ptr CNF -> Ptr CNFCtx -> IO ()

foreign import ccall unsafe "flint/nf_elem.h nf_elem_sub"
        nf_elem_sub :: Ptr CNF -> Ptr CNF -> Ptr CNF -> Ptr CNFCtx -> IO ()

foreign import ccall unsafe "flint/nf_elem.h nf_elem_mul"
        nf_elem_mul :: Ptr CNF -> Ptr CNF -> Ptr CNF -> Ptr CNFCtx -> IO ()

foreign import ccall unsafe "flint/nf_elem.h nf_elem_inv"
        nf_elem_inv :: Ptr CNF -> Ptr CNF -> Ptr CNFCtx -> IO ()

foreign import ccall unsafe "flint/nf_elem.h nf_elem_div"
        nf_elem_div :: Ptr CNF -> Ptr CNF -> Ptr CNF -> Ptr CNFCtx -> IO ()

foreign import ccall unsafe "flint/nf_elem.h nf_elem_pow"
        nf_elem_pow :: Ptr CNF -> Ptr CNF -> CULong -> Ptr CNFCtx -> IO ()
