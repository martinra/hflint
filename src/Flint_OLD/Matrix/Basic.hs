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

submatrix :: Int -> Int -> Int -> Int -> Matrix -> Matrix
submatrix = undefined

fromSubmatrices :: [(Int,Int,Matrix)] -> Maybe Matrix
fromSubmatrices = undefined

fromBlocks :: [[Matrix]] -> Maybe Matrix
fromBlocks = undefined

decomposeByPosSize :: [(Int,Int)] -> [(Int,Int)] -> Matrix -> [[Matrix]]
decomposeByPosSize = undefined

infixl 7 |*|
-- we assume that a and b are defined over the same 
(|*|) :: Matrix -> Matrix -> Matrix 
a |*| b | algA \= algB = error "Matrix multiplication only defined over equal algebras"
        | otherwise    = Matrix { components
                                      = fromEvaluations2 algebra a $
                                        zipWith FMPQMat.(|*|) (evaluations a) (evaluations b)
                                , nrows = nrows a
                                , ncols = ncols b
                                , algebra = algebra a }
