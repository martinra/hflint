{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , RankNTypes
  #-}

module HFlint.NMod.Context
where

import Data.Proxy
import Data.Word ( Word64 )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.Internal.Context
import HFlint.NMod.FFI


type ReifiesNModContext ctxProxy = ReifiesFlintContext NModCtx ctxProxy

withNModContext
 :: Word64
 -> (    forall ctxProxy .
         ReifiesFlintContext NModCtx ctxProxy
      => Proxy ctxProxy -> b)
 -> b
withNModContext n f = unsafePerformIO $ do
  ctx <- newFlintContext $ NModCtxData n
  return $ withFlintContext ctx f
