{-# LANGUAGE
    FlexibleContexts
  , FunctionalDependencies
  , MultiParamTypeClasses
  , TypeFamilies
  #-}

module HFlint.Internal.FlintPrim
  ( FlintPrim(..)
  )
where

import Foreign.Ptr ( Ptr )

import HFlint.Internal.Context
import HFlint.Internal.FlintWithContext ()


-- class of Flint types that do not require a context
class    FlintContext ctx
      => FlintPrim ctx (a :: * -> *) | a -> ctx
  where
  type CFlintPrim a :: *

  withFlintPrimCtx
    :: ReifiesFlintContext ctx ctxProxy
    => a ctxProxy
    -> (CFlintPrim a -> Ptr (CFlintContext ctx) -> IO b)
    -> IO b

  withNewFlintPrimCtx
    :: ReifiesFlintContext ctx ctxProxy
    => (Ptr (CFlintContext ctx) -> IO (CFlintPrim a))
    -> IO (a ctxProxy)

  withFlintPrim
    :: a ctxProxy
    -> (CFlintPrim a -> IO b)
    -> IO b
