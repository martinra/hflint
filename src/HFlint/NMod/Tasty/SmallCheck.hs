{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , UndecidableInstances
  #-}

module HFlint.NMod.Tasty.SmallCheck
where

import Test.SmallCheck.Series

import HFlint.NMod.Arithmetic ()
import HFlint.NMod.FFI
import HFlint.NMod.Context


instance ( ReifiesNModContext ctx, Monad m ) => Serial m (NMod ctx) where
  series = fmap fromIntegral (series :: Series m Int)
