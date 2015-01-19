module Flint.Matrix.Echelon
where

import Flint.FMPQMat

import Flint.Matrix.Internal
import Flint.Matrix.Echelon.EchelonHook
import Flint.Matrix.Echelon.Primitive

-- todo: rework this
echelonForm_ :: Matrix -> Matrix
echelonForm_ m | isTrivial (algebra m) = echelonFormQQ_ m
               | otherwise = undefined

echelonForm :: Matrix -> (Matrix, Matrix)
echelonForm m  | isTrivial (algebra m) = echelonFormQQ m
               | otherwise = undefined

echelonFormQQ_ :: Matrix -> Matrix
echelonFormQQ_ m = m { components = MatrixCoodinates [mc'] }
    where
      mc' = FMPQMat.echelonForm $ coordinates m
      
echelonFormQQ :: Matrix -> (Matrix, Matrix)
echelonFormQQ m = ( m { components = MatrixCoodinates [mcg'' !! 0] }
                   , mcg'' !! 1 )
    where
      mcg' = head $ FMPQMat.toBlocks [] [c] $
             FMPQMat.echelonForm $
             FMPQMat.fromBlocks $ [[coodinates m, FMPQMat.identityMatrix r c]]
      r = nrows m
      c = ncols m








                 
                  
echelonFormColumn_ :: Seq Matrix -> Seq [Matrix] -> -- row first
                      (Seq (Int, Matrix), Seq [Matrix])
echelonFormColumn_ bs rs = echelonFormColumn'_ (Sq.toList bs) (Sq.toList rs) Sq.empty Sq.empt

echelonFormColumn'_ :: [Matrix] -> [[Matrix]] -> -- row first
                       Seq (Int, Matrix) -> Seq [Matrix] ->
                       (Seq (Int, Matrix), Seq [Matrix])
echelonFormColumn'_ [] _ = (,)
echelonFormColumn'_ (b:bs) (r:rs) bsr rsr
    = echelonFormColumn'_ bs rs (bsr' |> (rk,b')) (rsr' |> r')
    where
      (b',u) = echelonFormPrimitive b
      r' = map (u|*|) r

      pivots = echelonFormPivots b'
      rk = length pivots
      
      (bsr', rsr') = Sq.zipWith (reduceBlock pivots b' r') bsr rsr

reduceBlock :: [Int] -> Matrix -> [Matrix] -> (Int,Matrix) -> [Matrix] -> (Matrix, [Matrix])
reduceBlock pivots a as (brk,b) bs = ( subtractRows pivotValues b a
                                     , zipWith (substractRows pivotValues) as bs )
    where
      pivotValues = flip map [0..(brk-1)] $ \i ->
                    flip map pivots $ \j ->
                    entry i j b


-- todo: move this into Matrix.Internal      
-- subtract from b rows of a determined by a ess:
-- the i-th element of ess determined what to substract from the i-th row of b
-- the i-th element of such an element determines which multiple of the i-th row of a should be subtracted
subtractRows :: [[AlgElement]] -> Matrix -> Matrix -> Matrix
subtractRows = undefined
