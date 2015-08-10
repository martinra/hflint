{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , RankNTypes
  #-}

module HFlint.NMod.Context
where

import Control.DeepSeq ( NFData, force )
import Data.Proxy
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.Internal.Context
import HFlint.NMod.FFI


type ReifiesNModContext ctxProxy = ReifiesFlintContext NModCtx ctxProxy

{-# NOINLINE withNModContext #-}
withNModContext
 :: NFData b
 => FlintLimb
 -> (    forall ctxProxy .
         ReifiesFlintContext NModCtx ctxProxy
      => Proxy ctxProxy -> b)
 -> b
withNModContext n f = unsafePerformIO $ do
  ctx <- newFlintContext $ NModCtxData n
  let h = force $ withFlintContext ctx f
  seq h $ freeFlintContext ctx
  return h
