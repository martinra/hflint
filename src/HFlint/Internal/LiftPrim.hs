{-# LANGUAGE
    BangPatterns
  , FlexibleContexts
  , ScopedTypeVariables
  #-}

module HFlint.Internal.LiftPrim
  ( liftFlintPrim
  , lift2FlintPrim
  )
where

import Foreign.Ptr ( Ptr )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.Internal.Context
import HFlint.Internal.FlintPrim
import HFlint.Internal.FlintWithContext ()


{-# INLINE liftFlintPrim #-}
liftFlintPrim
  :: ( FlintPrim ctx a, FlintPrim ctx b
     , ReifiesFlintContext ctx ctxProxy
     )
  => (    CFlintPrim a
       -> Ptr (CFlintContext ctx)
       -> IO (CFlintPrim b) )
  -> a ctxProxy
  -> b ctxProxy
liftFlintPrim f (!a) = unsafePerformIO $
    withFlintPrim a     $ \ca     ->
    withNewFlintPrimCtx $ \   ctx ->
    f ca ctx

{-# INLINE lift2FlintPrim #-}
lift2FlintPrim
  :: ( FlintPrim ctx c
     , FlintPrim ctx a, FlintPrim ctx b
     , ReifiesFlintContext ctx ctxProxy
     )
  => (    CFlintPrim a -> CFlintPrim b
       -> Ptr (CFlintContext ctx)
       -> IO (CFlintPrim c) )
  -> a ctxProxy -> b ctxProxy
  -> c ctxProxy
lift2FlintPrim f (!a) (!b) =
  unsafePerformIO $
    withFlintPrim a     $ \ca     ->
    withFlintPrim b     $ \cb     ->
    withNewFlintPrimCtx $ \   ctx ->
    f ca cb ctx
