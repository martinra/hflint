module Flint.Algebra
    ( Algebra(..)
    , degree
    )
where


import Flint.FMPQ

import Prelude hiding (length)
import Data.Vector

data Algebra = CyclicAlgebra { relation :: Vector FMPQ
                             }

degree :: Algebra -> Int
degree = length . relation
