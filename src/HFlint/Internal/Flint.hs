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
import Foreign.Ptr ( Ptr, nullPtr )

import HFlint.Internal.FlintWithContext


data FlintTrivialContext = FlintTrivialContext
instance FlintContext FlintTrivialContext where
  data CFlintCtx FlintTrivialContext

  implicitCtx f ctxptr aptr = runReaderT (f aptr) ctxptr

type CFlintTrivialContext = CFlintCtx FlintTrivialContext

runTrivialContext :: Monad m => ReaderT (Ptr CFlintTrivialContext) m a -> m a
runTrivialContext a = runReaderT a nullPtr 


class FlintWithContext FlintTrivialContext a => Flint a where
    newFlint :: IO a
    newFlint = runTrivialContext newFlintCtx 

    withFlint :: a
              -> (Ptr (CFlint a) -> IO b)
              -> IO (a, b)
    withFlint a f = runTrivialContext $ withFlintCtx a (const f)

    withFlint_ :: a
               -> (Ptr (CFlint a) -> IO b)
               -> IO a
    withFlint_ a f = runTrivialContext $ withFlintCtx_ a (const f)

    withNewFlint :: (Ptr (CFlint a) -> IO b)
                 -> IO (a, b)
    withNewFlint f = runTrivialContext $ withNewFlintCtx (const f)

    withNewFlint_ :: (Ptr (CFlint a) -> IO b)
                  -> IO a
    withNewFlint_ f = runTrivialContext $ withNewFlintCtx_ (const f)

