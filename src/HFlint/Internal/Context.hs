{-# LANGUAGE
    TypeFamilies
  #-}

module HFlint.Internal.Context
  ( FlintContext(..)

  , RFlint
  , RIOFlint

  , implicitCtx0
  , implicitCtx

  , FlintTrivialContext
  , CFlintTrivialContext
  , runTrivialContext
  )
where

import Control.Monad.Trans.Reader
import Foreign.ForeignPtr ( ForeignPtr, newForeignPtr_ )
import Foreign.Ptr ( Ptr, nullPtr )


class FlintContext ctx where
  data CFlintCtx ctx :: *
  data FlintContextData ctx :: *

  newFlintContext
    :: FlintContextData ctx
    -> IO ctx

  withFlintContext
    :: ctx
    -> ( Ptr (CFlintCtx ctx) -> b )
    -> b

type RFlint ctx = Reader (Ptr (CFlintCtx ctx))
type RIOFlint ctx = ReaderT (Ptr (CFlintCtx ctx)) IO


{-# INLINE implicitCtx0 #-}
implicitCtx0
  :: RIOFlint ctx a
  -> Ptr (CFlintCtx ctx) -> IO a
implicitCtx0 a ctxptr = runReaderT a ctxptr

{-# INLINE implicitCtx #-}
implicitCtx
  :: ( a -> RIOFlint ctx b )
  -> a -> Ptr (CFlintCtx ctx) -> IO b
implicitCtx f a ctxptr = runReaderT (f a) ctxptr


data FlintTrivialContext = FlintTrivialContext
type CFlintTrivialContext = CFlintCtx FlintTrivialContext

instance FlintContext FlintTrivialContext where
  data CFlintCtx FlintTrivialContext
  data FlintContextData FlintTrivialContext

  {-# INLINE newFlintContext #-}
  newFlintContext = const $ return FlintTrivialContext

  {-# INLINE withFlintContext #-}
  withFlintContext _ f = f nullPtr


{-# INLINE runTrivialContext #-}
runTrivialContext :: Monad m => ReaderT (Ptr CFlintTrivialContext) m a -> m a
runTrivialContext a = runReaderT a nullPtr 

