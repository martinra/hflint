{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , RankNTypes
  #-}

module HFlint.NMod.Context
where

import Data.Proxy
import Data.Reflection
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


newtype Modulus a = Modulus a
  deriving ( Eq, Show )

{-# INLINE modulus #-}
modulus :: forall ctx . ReifiesNModContext ctx => Proxy ctx -> Modulus Word64
modulus = modulusIntegral

{-# INLINE modulusIntegral #-}
modulusIntegral
  :: forall ctx a .
     ( ReifiesNModContext ctx, Integral a )
  => Proxy ctx -> Modulus a
modulusIntegral proxy = Modulus $ fromIntegral $ unsafePerformIO $ nmod_n (reflect proxy)
