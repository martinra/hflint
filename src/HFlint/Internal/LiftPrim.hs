{-# LANGUAGE
    BangPatterns
  #-}

module HFlint.Internal.LiftPrim
  ( liftFlintPrim
  , lift2FlintPrim
  )
where

import Foreign.Ptr ( Ptr )

import HFlint.Internal.Context
import HFlint.Internal.FlintPrim
import HFlint.Internal.FlintWithContext ()
import HFlint.Internal.Lift.Utils


{-# INLINE liftFlintPrim #-}
liftFlintPrim
  :: ( FlintPrim ctx a, FlintPrim ctx b )
  => (    CFlintPrim a
       -> Ptr (CFlintCtx ctx)
       -> IO (CFlintPrim b) )
  -> RFlint ctx a
  -> RFlint ctx b
liftFlintPrim f (!a) = do
  a' <- a
  fromIO $
    withFlintPrimImplicitCtx a' $ \ca     ->
    withNewFlintPrim            $ \   ctx ->
    f ca ctx

{-# INLINE lift2FlintPrim #-}
lift2FlintPrim
  :: ( FlintPrim ctx c
     , FlintPrim ctx a, FlintPrim ctx b )
  => (    CFlintPrim a -> CFlintPrim b
       -> Ptr (CFlintCtx ctx)
       -> IO (CFlintPrim c) )
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx c
lift2FlintPrim f (!a) (!b) = do
  a' <- a; b' <- b
  fromIO $
    withFlintPrimImplicitCtx a' $ \ca     ->
    withFlintPrimImplicitCtx b' $ \cb     ->
    withNewFlintPrim            $ \   ctx ->
    f ca cb ctx
