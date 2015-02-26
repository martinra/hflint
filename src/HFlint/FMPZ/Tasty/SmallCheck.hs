{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  #-}

module HFlint.FMPZ.Tasty.SmallCheck
where

import Test.SmallCheck.Series

import HFlint.FMPZ.Arithmetic ()
import HFlint.FMPZ.FFI


-- SmallCheck is hiding all instances. `suchThat` as soon as iit
-- is available
instance Monad m => Serial m FMPZ where
  series = generate $ \d -> map fromIntegral [-d..d]
