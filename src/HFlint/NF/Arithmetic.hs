{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , InstanceSigs
  , ScopedTypeVariables
  , TypeSynonymInstances
  , UndecidableInstances
  #-}

module HFlint.NF.Arithmetic
where

import System.IO.Unsafe ( unsafePerformIO )

import HFlint.Internal.LiftCtx
import HFlint.FMPQ
import HFlint.FMPZ
import HFlint.NF.Context
import HFlint.NF.FFI


instance    ReifiesNFContext ctxProxy
         => Num (NF ctxProxy) where
  {-# INLINE fromInteger #-}
  fromInteger a = unsafePerformIO $
    withNewNF_                 $ \bptr ctxptr ->
    withFMPZ_ (fromInteger a)  $ \aptr        ->
      nf_elem_set_fmpz bptr aptr ctxptr

  {-# INLINE (+) #-}
  (+) = lift2FlintCtx_ nf_elem_add 
  {-# INLINE (-) #-}
  (-) = lift2FlintCtx_ nf_elem_sub
  {-# INLINE (*) #-}
  (*) = lift2FlintCtx_ nf_elem_mul

  {-# INLINE negate #-}
  negate = liftFlintCtx_ nf_elem_neg
  {-# INLINE abs #-}
  abs = error "NF.abs"
  {-# INLINE signum #-}
  signum = error "NF.signum"

instance    ReifiesNFContext ctxProxy
         => Fractional (NF ctxProxy) where
  {-# INLINE (/) #-}
  (/) = lift2FlintCtx_ nf_elem_div
  {-# INLINE recip #-}
  recip = liftFlintCtx_ nf_elem_inv

  fromRational a = unsafePerformIO $
    withNewNF_                 $ \bptr ctxptr ->
    withFMPQ_ (fromRational a) $ \aptr        ->
      nf_elem_set_fmpq bptr aptr ctxptr
