{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  #-}

module HFlint.FMPQ.Tasty.SmallCheck
where

import Control.Monad ( guard, (=<<) )
import Math.Structure ( Unit(..), isUnit )
import Test.SmallCheck.Series

import HFlint.FMPQ.Algebra ()
import HFlint.FMPQ.Arithmetic ()
import HFlint.FMPQ.FFI


instance Monad m => Serial m FMPQ where
  series = fmap fromRational (series :: Series m Rational)

instance Monad m => Serial m (Unit FMPQ) where
  series = do
    a <- series
    guard $ not $ isUnit a
    return $ Unit a
