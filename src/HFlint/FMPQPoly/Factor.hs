{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module HFlint.FMPQPoly.Factor
where

import Control.Arrow ( first, (***) )
import qualified Data.Vector as V

import HFlint.Internal.Factor

import HFlint.FMPQ
import HFlint.FMPQPoly.Basic
import HFlint.FMPQPoly.FFI
import HFlint.FMPQPoly.Internal
import HFlint.FMPZPoly


instance Factorizable FMPQPoly FMPQ where
  factor a = Factored (fromFMPZs u d) (V.map (first fromFMPZPoly) f)
    where
    (d, a') = toFMPZPoly a
    Factored u f = factor a' 
