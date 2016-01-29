{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module HFlint.Internal.FlintWithContext
  ( FlintWithContext(..)
  )
where

import Foreign.Ptr ( Ptr )

import HFlint.Internal.Context


class FlintContext ctx
      => FlintWithContext ctx (a :: * -> *) | a -> ctx
  where
  data CFlintCtx a :: *

  newFlintCtx
    :: ReifiesFlintContext ctx ctxProxy
    => IO (a ctxProxy)

  withFlintCtx
    :: ReifiesFlintContext ctx ctxProxy
    => a ctxProxy
    -> (Ptr (CFlintCtx a) -> Ptr (CFlintContext ctx) -> IO b)
    -> IO (a ctxProxy, b)

  {-# INLINE withNewFlintCtx #-}
  withNewFlintCtx
    :: ReifiesFlintContext ctx ctxProxy
    => (Ptr (CFlintCtx a) -> Ptr (CFlintContext ctx) -> IO b)
    -> IO (a ctxProxy, b)
  withNewFlintCtx f = flip withFlintCtx f =<< newFlintCtx


  {-# INLINE withFlintCtx_ #-}
  withFlintCtx_
    :: ReifiesFlintContext ctx ctxProxy
    => a ctxProxy
    -> (Ptr (CFlintCtx a) -> Ptr (CFlintContext ctx) -> IO b)
    -> IO (a ctxProxy)
  withFlintCtx_ a f = fst <$> withFlintCtx a f


  {-# INLINE withNewFlintCtx_ #-}
  withNewFlintCtx_
    :: ReifiesFlintContext ctx ctxProxy
    => (Ptr (CFlintCtx a) -> Ptr (CFlintContext ctx) -> IO b)
    -> IO (a ctxProxy)
  withNewFlintCtx_ f = fst <$> withNewFlintCtx f
