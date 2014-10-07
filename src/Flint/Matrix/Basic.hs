module Flint.Matrix.Basic
where

import Flint.Algebra
import Flint.Matrix.Internal

import Flint.FMPQMat.Basic


zeroMatrix :: Int -> Int -> Algebra -> Matrix
zeroMatrix r c alg = Matrix { components = MatrixCoordinates $
                                           replicate (degree alg) $ zeroFMPQMat r c
                            , nrows = r
                            , ncols = c
                            , algebra = alg}

oneMatrix :: Int -> Int -> Algebra -> Matrix
oneMatrix r c alg = Matrix { components = MatrixCoordinates $
                                          (:) (oneFMPQMat r c) $
                                          replicate (degree alg - 1) $ zeroFMPQMat r c
                            , nrows = r
                            , ncols = c
                            , algebra = alg}

fromSubmatrices :: [(Int,Int,Matrix)] -> Maybe Matrix
fromSubmatrices = undefined
