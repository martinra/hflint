{-# LANGUAGE
    TypeFamilies
  , MultiParamTypeClasses
  , FunctionalDependencies
  #-}

module HFlint.Internal.FlintWithContext
  ( FlintWithContext(..)
  )
where

import Foreign.Ptr ( Ptr )

import HFlint.Internal.Context
import HFlint.Internal.Flint


-- class of Flint types that do not require a context
class ( FlintContext ctx, Flint a )
      => FlintWithContext ctx a | a -> ctx
  where

  withFlintCtx
    :: a
    -> (Ptr (CFlint a) -> Ptr (CFlintCtx ctx) -> IO b)
    -> IO (a, b)

  {-# INLINE withNewFlintCtx #-}
  withNewFlintCtx
    :: (Ptr (CFlint a) -> Ptr (CFlintCtx ctx) -> IO b)
    -> IO (a, b)
  withNewFlintCtx f = flip withFlintCtx f =<< newFlint


  {-# INLINE withFlintCtx_ #-}
  withFlintCtx_
    :: a
    -> (Ptr (CFlint a) -> Ptr (CFlintCtx ctx) -> IO b)
    -> IO a
  withFlintCtx_ a f = fst <$> withFlintCtx a f


  {-# INLINE withNewFlintCtx_ #-}
  withNewFlintCtx_
    :: (Ptr (CFlint a) -> Ptr (CFlintCtx ctx) -> IO b)
    -> IO a
  withNewFlintCtx_ f = fst <$> withNewFlintCtx f
