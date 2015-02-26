{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  #-}

module HFlint.FMPQ.Tasty.SmallCheck
where

import Test.SmallCheck.Series

import HFlint.FMPQ.Arithmetic ()
import HFlint.FMPQ.FFI


instance Monad m => Serial m FMPQ where
  series = fmap fromRational (series :: Series m Rational)
