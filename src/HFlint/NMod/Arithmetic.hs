{-# LANGUAGE
    FlexibleInstances
  , TypeSynonymInstances
  #-}

module HFlint.NMod.Arithmetic
where

import Control.Monad.Reader
import Data.Ratio ( numerator, denominator )

import HFlint.Internal.LiftPrim
import HFlint.Internal.Lift.Utils
import HFlint.NMod.FFI


instance Num RNMod where
  {-# INLINE fromInteger #-}
  fromInteger a = 
    fromIO $ do
      ctx <- ask
      n <- liftIO $ nmod_n ctx
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

instance Fractional RNMod where
  {-# INLINE (/) #-}
  (/) = lift2FlintPrim nmod_div
  {-# INLINE recip #-}
  recip = liftFlintPrim nmod_inv

  fromRational a = n / d
    where
      n = fromInteger $ numerator a
      d = fromInteger $ denominator a
