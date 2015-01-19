module Flint.Algebra
    ( Algebra(..)
    , degree
    )
where


import Flint.FMPQ

import Prelude hiding (length)
import Data.Vector

data Algebra = QQ
             | CyclicAlgebra { relation :: Vector FMPQ
                             }

degree :: Algebra -> Int
degree QQ = 1
degree alg@(CyclicAlgebra _) = length $ relation alg
