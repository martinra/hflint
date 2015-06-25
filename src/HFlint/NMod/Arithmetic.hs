{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , InstanceSigs
  , ScopedTypeVariables
  , TypeSynonymInstances
  , UndecidableInstances
  #-}

module HFlint.NMod.Arithmetic
where

import Data.Proxy
import Data.Reflection
import Data.Ratio ( numerator, denominator )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.Internal.Context
import HFlint.Internal.LiftPrim
import HFlint.NMod.FFI


instance    ReifiesFlintContext NModCtx ctxProxy
         => Num (NMod ctxProxy) where
  {-# INLINE fromInteger #-}
  fromInteger a = unsafePerformIO $ do
    n <- nmod_n $ reflect (Proxy :: Proxy ctxProxy)
    return $ NMod $ fromInteger $
      a `mod` fromIntegral n

  {-# INLINE (+) #-}
  (+) = lift2FlintPrim nmod_add 
  {-# INLINE (-) #-}
  (-) = lift2FlintPrim nmod_sub
  {-# INLINE (*) #-}
  (*) = lift2FlintPrim nmod_mul

  {-# INLINE negate #-}
  negate = liftFlintPrim nmod_neg
  {-# INLINE abs #-}
  abs = error "RNMod.abs"
  {-# INLINE signum #-}
  signum = error "RNMod.signum"

instance    ReifiesFlintContext NModCtx ctxProxy
         => Fractional (NMod ctxProxy) where
  {-# INLINE (/) #-}
  (/) = lift2FlintPrim nmod_div
  {-# INLINE recip #-}
  recip = liftFlintPrim nmod_inv

  fromRational a = n / d
    where
      n = fromInteger $ numerator a
      d = fromInteger $ denominator a
