{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , RankNTypes
  #-}

module HFlint.NF.Context
where

import Control.DeepSeq ( NFData, force )
import Data.Proxy
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.Internal.Context
import HFlint.FMPQPoly
import HFlint.NF.FFI


type ReifiesNFContext ctxProxy = ReifiesFlintContext NFCtx ctxProxy

{-# NOINLINE withNFContext #-}
withNFContext
 :: NFData b
 => FMPQPoly
 -> (    forall ctxProxy .
         ReifiesNFContext ctxProxy
      => Proxy ctxProxy -> b)
 -> b
withNFContext p f = unsafePerformIO $ do
  -- todo: check that p is admissible
  ctx <- newFlintContext $ NFCtxData p
  let h = force $ withFlintContext ctx f
  seq h $ freeFlintContext ctx
  return h
