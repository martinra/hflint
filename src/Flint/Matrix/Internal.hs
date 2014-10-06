module Flint.Matrix.Internal
where

import Flint.FMPQMat
import Flint.Algebra

import Flint.FMPQMat.FFI

import qualified Data.DList as DL
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

-- Matrices over semisimple algebras
data Matrix = Matrix { components :: MatrixComponents
                     , nrows :: Int
                     , ncols :: Int
                     , algebra :: Algebra
                     }

data MatrixComponents = MatrixCoordinates [FMPQMat]
---                      | MatrixEvaluations [FMPQMat]


coordinates :: Matrix -> [FMPQMat]
coordinates m = case components m of
                  MatrixCoordinates mc -> mc

withNewMatrixCoordinates :: Int -> Int -> Algebra -> ([Ptr CFMPQMat] -> IO ()) -> Matrix
withNewMatrixCoordinates r c alg f =
    Matrix { components = go (degree alg) (\aptrs -> f aptrs >> return DL.empty)
           , nrows = r
           , ncols = c
           , algebra = alg }
    where
      go :: Int -> ([Ptr CFMPQMat] -> IO (DL.DList FMPQMat)) -> MatrixComponents
      go 0 g = MatrixCoordinates $ DL.toList $ unsafePerformIO $ g []
      go d g = go (d-1) $ \aptrs ->
               return . uncurry (flip DL.snoc) =<<
               (withNewFMPQMat r c $ const $ \aptr -> g (aptr:aptrs))
