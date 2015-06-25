{-# LANGUAGE
    TypeFamilies
  , MultiParamTypeClasses
  , FunctionalDependencies
  #-}

module HFlint.Internal.FlintPrim
  ( FlintPrim(..)
  )
where

import Foreign.Ptr ( Ptr )

import HFlint.Internal.Context
import HFlint.Internal.FlintWithContext ()


-- class of Flint types that do not require a context
class FlintContext ctx => FlintPrim ctx a | a -> ctx where
  type CFlintPrim a :: *

  withFlintPrim
    :: a
    -> (CFlintPrim a -> Ptr (CFlintCtx ctx) -> IO b)
    -> RIOFlint ctx b

  withNewFlintPrim
    :: (Ptr (CFlintCtx ctx) -> IO (CFlintPrim a))
    -> RIOFlint ctx a


  {-# INLINE withFlintPrimImplicitCtx #-}
  withFlintPrimImplicitCtx
    :: a
    -> (CFlintPrim a -> RIOFlint ctx b)
    -> RIOFlint ctx b
  withFlintPrimImplicitCtx a f = withFlintPrim a $ implicitCtx f

  {-# INLINE withNewFlintPrimImplicitCtx #-}
  withNewFlintPrimImplicitCtx
    :: RIOFlint ctx (CFlintPrim a)
    -> RIOFlint ctx a
  withNewFlintPrimImplicitCtx f = withNewFlintPrim $ implicitCtx0 f
