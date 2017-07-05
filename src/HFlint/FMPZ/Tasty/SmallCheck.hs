{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  #-}

module HFlint.FMPZ.Tasty.SmallCheck
where

import Control.Monad ( guard )
import Math.Structure ( Unit(..), isUnit )
import Test.SmallCheck.Series

import HFlint.FMPZ.Algebra ()
import HFlint.FMPZ.Arithmetic ()
import HFlint.FMPZ.FFI


instance Monad m => Serial m FMPZ where
  series = fmap fromInteger (series :: Series m Integer)

instance Monad m => Serial m (Unit FMPZ) where
  series = do
    a <- series
    guard $ isUnit a
    return $ Unit a
