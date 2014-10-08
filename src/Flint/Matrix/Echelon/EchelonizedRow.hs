module Flint.Matrix.Echelon.EchelonizedRow
where

import Flint.Matrix.Internal

import Flint.FMPQMat.FFI
import Flint.FMPQMat.Internal

import Prelude hiding (foldl)

import Data.Foldable (foldl)
import Data.Sequence (Seq)
import qualified Data.Sequence as Sq
import Foreign.Ptr (Ptr)

import Control.Applicative ((<$>))
import Control.Monad (zipWithM_)


newtype EchelonizedRow = EchelonizedRow [Matrix]

headWithDefault :: a -> (Matrix -> a) -> EchelonizedRow -> a
headWithDefault d f (EchelonizedRow ms) =
    case ms of
      m:_ -> f m
      []  -> d

rowCOffset :: EchelonizedRow -> Int
rowCOffset = headWithDefault 0 ncols

rowROffset :: EchelonizedRow -> Int
rowROffset = headWithDefault 0 nrows


mergeEchelonizedRows :: Seq EchelonizedRow -> Maybe Matrix
mergeEchelonizedRows ers | Sq.null ers = Nothing
                         | otherwise
                             = flip (withNewMatrixCoordinates r c) f <$> mbAlg
    where
      (r,c,f) = mergeEchelonizedRows' ers
      mbAlg = headWithDefault Nothing (Just . algebra) $ Sq.index ers 0

mergeEchelonizedRows' :: Seq EchelonizedRow -> (Int, Int, [Ptr CFMPQMat] -> IO ())
mergeEchelonizedRows' = foldl step (0,0,const $ return ())
    where
      step (r,c,f) h = ( r + rowROffset h, c + rowCOffset h
                       , extendFByEchelonizedRow r c f h )

extendFByEchelonizedRow :: Int -> Int -> ([Ptr CFMPQMat] -> IO ()) -> EchelonizedRow ->
                 ([Ptr CFMPQMat] -> IO ())
extendFByEchelonizedRow r c f (EchelonizedRow rs) = f'
    where
      (_,_,f') = foldl step (r,c,f) rs
      step (r',c',g) m = ( r', c' + ncols m
                         , extendFByBlock r' c' g m )

extendFByBlock :: Int -> Int -> ([Ptr CFMPQMat] -> IO ()) -> Matrix ->
                  [Ptr CFMPQMat] -> IO ()
extendFByBlock r c f m aptrs = do
  f aptrs
  zipWithM_ set aptrs $ coordinates m
    where
      set aptr mc = withFMPQMat_ mc $ const $ \mcptr ->
                    setSubmatrix aptr r' c' mcptr
      r' = fromIntegral r
      c' = fromIntegral c
