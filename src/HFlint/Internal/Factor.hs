{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  #-}

module HFlint.Internal.Factor
where

import Data.Vector ( Vector )
import Numeric.Natural ( Natural )


data Factored a b = Factored b (Vector (a,Natural))

class Factorizable a b | a -> b where
  factor :: a -> Factored a b
