module Flint.Matrix.Echelon.EchelonFormByBlocks
where

import Flint.Matrix.Internal
import Flint.Matrix.Basic

import Flint.Matrix.Echelon.EchelonFormPrimitive
import Flint.Matrix.Echelon.EchelonizedRow


import Control.Arrow ((&&&), (***), first)
import Data.Maybe (fromJust)
import Data.Foldable (toList, foldMap)
import Data.Sequence (Seq, (|>), viewl, viewr, ViewL(..), ViewR(..))
import qualified Data.Sequence as Sq
import Data.Monoid()

import Safe

echelonDecomposition :: EchelonParameter ->
                        Matrix -> Seq [Matrix]
echelonDecomposition ep m = Sq.fromList $ decomposeByPosSize
                            (rowDecompositionPosSize ep $ nrows m)
                            (colDecompositionPosSize ep $ ncols m)
                            m

echelonFormByBlocks_ :: EchelonParameter ->
                        Seq [Matrix] ->
                        Seq EchelonizedRow -> Maybe Matrix
echelonFormByBlocks_ ep ed ers | Sq.null ed = mergeEchelonizedRows ers
                               | otherwise  = echelonFormByBlocks_ ep ed'' (ers|>er)
    where
      (bs,rs) = foldMap (Sq.singleton *** Sq.singleton) $
                fmap (first head . splitAt 1) ed
      (_,kr , bs',rs', ed') = echelonFormColumn_ bs rs
      ed'' = echelonRedecomposition ep kr ed'

      er = EchelonizedRow $ map (fromJust . fromBlocks) $
           takeWhile (not . null . headDef []) $ map fst $
           iterate step ([], Sq.zipWith (:) bs' rs')
               where
                 step = foldMap ( (:[]) *** Sq.singleton) .
                        fmap (splitAt 1) . snd


data EchelonParameter = EchelonParameter
    { _echelonRowCutOff :: Int
    , _echelonColCutOff :: Int
    }

defaultEchelonParameter :: EchelonParameter
defaultEchelonParameter
    = EchelonParameter
      { _echelonRowCutOff = 64
      , _echelonColCutOff = 128
      }

rowDecompositionPosSize :: EchelonParameter -> Int -> [(Int,Int)]
rowDecompositionPosSize ep = decompositionPosSize (_echelonRowCutOff ep)

colDecompositionPosSize :: EchelonParameter -> Int -> [(Int,Int)]
colDecompositionPosSize ep = decompositionPosSize (_echelonColCutOff ep)

-- Position and size of decomposition into pieces of Size at most maxS
decompositionPosSize :: Int -> Int -> [(Int, Int)]
decompositionPosSize maxS totalS | totalS <= 0 = []
                                 | otherwise
    = [(ix*width, width) | ix <- [0..(nBlocks-2)]] ++
      [((nBlocks-1)*width, totalS - (nBlocks-1)*width)]
          where
            width = 1 + div (totalS-1) nBlocks
            nBlocks = 1 + div (totalS-1) maxS


--      (_,rk' , bs',rs', ed') = echelonFormColumn_ bs rs
echelonFormColumn_ :: Seq Matrix -> Seq [Matrix] ->
                      ( Int,Int, Seq Matrix,Seq [Matrix], Seq [Matrix] )
echelonFormColumn_ = echelonFormColumn'_ (0,0, Sq.empty,Sq.empty, Sq.empty)
    where
      echelonFormColumn'_ rt@(rk,kr, bsr,rsr, ed) bs rs
          = case viewl bs of
              EmptyL -> rt
              b:<bs' -> echelonFormColumn'_ (rk',kr', bsr'|>bu,rsr'|>ru, ed|>rl) bs' rs'
                  where
                    rb = nrows b
                    cb = ncols b

                    r:<rs' = viewl rs
                            
                    rk' = rk+brk
                    kr' = kr+rb-brk

                    (bg,g) = echelonFormPrimitive b
                    brk = echelonFormRank bg
                    
                    bu = submatrix 0 0 brk cb bg
                    (ru,rl) = unzip $
                              map ((submatrix 0 0 brk cb &&&
                                    submatrix brk 0 (rb-brk) cb) . (g|*|))
                              r

                    (bsr',rsr') = foldMap (Sq.singleton *** Sq.singleton) $
                                  Sq.zipWith (reduceRow bu ru) bsr rsr

reduceRow :: Matrix -> [Matrix] -> Matrix -> [Matrix] ->
             (Matrix, [Matrix])
reduceRow = undefined

echelonRedecomposition :: EchelonParameter ->
                          Int -> Seq [Matrix] ->
                          Seq [Matrix]
echelonRedecomposition ep r ed
    = fmap (map $ fromJust . fromBlocks . map (:[]) . toList) $
      echelonRedecomposition' (map snd $ rowDecompositionPosSize ep $ r) ed Sq.empty

echelonRedecomposition' :: [Int] -> Seq [Matrix] -> Seq [Seq Matrix] -> Seq [Seq Matrix]
echelonRedecomposition' [] _ bss = bss
echelonRedecomposition' (r:rs) ed bss = echelonRedecomposition' rs' ed' bss''
    where
      ed':>ms = viewr ed
      bss':>bs = viewr bss

      rm = nrows $ head ms
      cm = ncols $ head ms

      rs' | rm >= r   = rs
          | otherwise = (r-rm):rs

      bss'' | rm < r    = bss'|>(zipWith (|>) bs ms)
            | rm == r   = bss'|>(zipWith (|>) bs ms)|>[]
            | otherwise = bss'|>(zipWith (|>) bs mus)|>(map Sq.singleton mls)

      -- this is only evaluated in case rm > r (see bss'')
      mus = flip map ms $ submatrix 0 0 r cm
      mls = flip map ms $ submatrix r 0 (rm-r) cm
