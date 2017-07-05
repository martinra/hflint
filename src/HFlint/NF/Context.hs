module HFlint.NF.Context
where

import HFlint.Utility.Prelude

import Control.Exception.Safe ( MonadMask )
import Control.Monad.IO.Class ( MonadIO )

import HFlint.Internal.Context
import HFlint.FMPQPoly
import HFlint.NF.FFI


type ReifiesNFContext ctxProxy = ReifiesFlintContext NFCtx ctxProxy

withNFContext
 :: NFData b
 => FMPQPoly
 -> (    forall ctxProxy .
         ReifiesNFContext ctxProxy
      => Proxy ctxProxy -> b)
 -> b
withNFContext = withFlintContextFromData NFCtxData

withNFContextM
 :: ( MonadIO m, MonadMask m )
 => FMPQPoly
 -> (    forall ctxProxy .
         ReifiesNFContext ctxProxy
      => Proxy ctxProxy -> m b)
 -> m b
withNFContextM = withFlintContextFromDataM NFCtxData
