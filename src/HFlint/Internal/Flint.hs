{-# LANGUAGE
    EmptyDataDecls
  , TypeFamilies
  , FlexibleContexts
  , FlexibleInstances
  , ScopedTypeVariables
  #-}

module HFlint.Internal.Flint
  ( Flint(..)

 )
where

import Foreign.Ptr ( Ptr )

import HFlint.Internal.Context
import HFlint.Internal.FlintWithContext
import HFlint.Internal.Lift.Utils


class FlintWithContext FlintTrivialContext a => Flint a where
  {-# INLINE newFlint #-}
  newFlint :: IO a
  newFlint = runTrivialContext newFlintCtx 

  {-# INLINE withFlint #-}
  withFlint :: a
            -> (Ptr (CFlint a) -> IO b)
            -> IO (a, b)
  withFlint a f = runTrivialContext $ withFlintCtx a (constBack f)

  {-# INLINE withFlint_ #-}
  withFlint_ :: a
             -> (Ptr (CFlint a) -> IO b)
             -> IO a
  withFlint_ a f = runTrivialContext $ withFlintCtx_ a (constBack f)

  {-# INLINE withNewFlint #-}
  withNewFlint :: (Ptr (CFlint a) -> IO b)
               -> IO (a, b)
  withNewFlint f = runTrivialContext $ withNewFlintCtx (constBack f)

  {-# INLINE withNewFlint_ #-}
  withNewFlint_ :: (Ptr (CFlint a) -> IO b)
                -> IO a
  withNewFlint_ f = runTrivialContext $ withNewFlintCtx_ (constBack f)
