{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , UndecidableInstances
  #-}

module HFlint.NMod.Tasty.SmallCheck
where

import Control.Monad ( guard )
import Math.Structure ( Unit(..), DecidableUnit(..) )
import Test.SmallCheck.Series

import HFlint.NMod.Algebra ()
import HFlint.NMod.Arithmetic ()
import HFlint.NMod.FFI
import HFlint.NMod.Context


instance ( ReifiesNModContext ctx, Monad m ) => Serial m (NMod ctx) where
  series = fmap fromIntegral (series :: Series m Int)

instance ( ReifiesNModContext ctx, Monad m ) => Serial m (Unit (NMod ctx)) where
  series = do
    a <- series
    guard $ not $ isUnit a
    return $ Unit a
