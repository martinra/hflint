{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , RankNTypes
  , MultiParamTypeClasses
  , TypeFamilies
  #-}

module HFlint.Internal.Context
  ( FlintContext(..)
  , ReifiesFlintContext
  , withFlintContextFromData
  , withFlintContextFromDataM
  )
where

import HFlint.Utility.Prelude

import Control.Exception.Safe ( MonadMask, bracket )
import Control.Monad.IO.Class ( MonadIO, liftIO )


type ReifiesFlintContext ctx ctxProxy =
  ( FlintContext ctx, Reifies ctxProxy (Ptr (CFlintContext ctx)) )

class FlintContext ctx where
  data CFlintContext ctx :: *
  data FlintContextData ctx :: *

  newFlintContext
    :: FlintContextData ctx
    -> IO ctx

  freeFlintContext
    :: ctx
    -> IO ()

  withFlintContext
    :: ctx
    -> (    forall ctxProxy .
            ReifiesFlintContext ctx ctxProxy
         => Proxy ctxProxy -> b)
    -> b


withFlintContextFromData
 :: ( FlintContext ctx, NFData b )
 => ( a -> FlintContextData ctx )
 -> a
 -> (    forall ctxProxy .
         ReifiesFlintContext ctx ctxProxy
      => Proxy ctxProxy -> b)
 -> b
withFlintContextFromData init a f = unsafePerformIO $
  bracket (newFlintContext $ init a) freeFlintContext $ \ctx ->
    pure $ force $ withFlintContext ctx f

withFlintContextFromDataM
 :: ( FlintContext ctx, MonadIO m, MonadMask m )
 => ( a -> FlintContextData ctx )
 -> a
 -> (    forall ctxProxy .
         ReifiesFlintContext ctx ctxProxy
      => Proxy ctxProxy -> m b)
 -> m b
withFlintContextFromDataM init a f =
  bracket (liftIO $ newFlintContext $ init a) (liftIO . freeFlintContext) $ \ctx ->
    withFlintContext ctx f
