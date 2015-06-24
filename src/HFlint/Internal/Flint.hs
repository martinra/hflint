{-# LANGUAGE
    EmptyDataDecls
  , TypeFamilies
  , FlexibleContexts
  , FlexibleInstances
  , ScopedTypeVariables
  #-}

module HFlint.Internal.Flint
  ( Flint(..)

  , FlintTrivialContext
  , CFlintTrivialContext
  , runTrivialContext
  )
where

import Control.Monad.Reader
import Foreign.ForeignPtr ( newForeignPtr_ )
import Foreign.Ptr ( Ptr, nullPtr )

import HFlint.Internal.FlintWithContext
import HFlint.Internal.Utils


data FlintTrivialContext = FlintTrivialContext

instance FlintContext FlintTrivialContext where
  data CFlintCtx FlintTrivialContext

  {-# INLINE newFlintContext #-}
  newFlintContext = newForeignPtr_ nullPtr

  {-# INLINE implicitCtx #-}
  implicitCtx f aptr ctxptr = runReaderT (f aptr) ctxptr

type CFlintTrivialContext = CFlintCtx FlintTrivialContext

{-# INLINE runTrivialContext #-}
runTrivialContext :: Monad m => ReaderT (Ptr CFlintTrivialContext) m a -> m a
runTrivialContext a = runReaderT a nullPtr 


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
