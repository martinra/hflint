module HFlint.NMod.Context
where

import HFlint.Utility.Prelude

import Control.Exception.Safe ( MonadMask )
import Control.Monad.IO.Class ( MonadIO )

import HFlint.Internal.Context
import HFlint.NMod.FFI


type ReifiesNModContext ctxProxy = ReifiesFlintContext NModCtx ctxProxy

withNModContext
 :: NFData b
 => FlintLimb
 -> (    forall ctxProxy .
         ReifiesNModContext ctxProxy
      => Proxy ctxProxy -> b)
 -> b
withNModContext = withFlintContextFromData NModCtxData

withNModContextM
 :: ( MonadIO m, MonadMask m )
 => FlintLimb
 -> (    forall ctxProxy .
         ReifiesNModContext ctxProxy
      => Proxy ctxProxy -> m b)
 -> m b
withNModContextM = withFlintContextFromDataM NModCtxData
