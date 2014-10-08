module Flint.Matrix.Echelon.EchelonFormPrimitive
where

import qualified Flint.FMPQMat as FMPQMat
import Flint.Matrix.Internal

echelonFormPrimitive :: Matrix -> (Matrix,Matrix)
echelonFormPrimitive m = (submatrix 0 0 r c m'', submatrix 0 r r r m'')
    where
      m' = echelonFormPrimitive_ $ fromBlocks [[m, oneMatrix r r]]
      r = nrows m
      c = ncols m

echelonFormPrimitive_ :: Matrix -> Matrix
echelonFormPrimitive_ m | isQQ $ algebra m 1
                            = m { components = MatrixCoordinates [mc'] }
                              mc' = FMPQMat.echelonForm $ head $ coordinates m
                        | otherwise          = undefined

echelonFormRank :: Matrix -> Int
echelonFormRank = FMPQMat.echelonFormRank $ head $ coordinates










