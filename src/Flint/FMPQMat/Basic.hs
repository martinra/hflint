module Flint.FMPQMat.Basic
where


import Flint.Internal.FlintMat
import Flint.FMPQ.FFI
import Flint.FMPQ.Internal

import Flint.FMPQMat.FFI
import Flint.FMPQMat.Internal

import Prelude hiding (length)

import Data.Foldable (forM_)
import Data.Vector (Vector, (!), indexed, length)

import System.IO.Unsafe (unsafePerformIO)


fromVVector :: Vector (Vector FMPQ) -> Maybe FMPQMat
fromVVector rows | length rows == 0 = Nothing
                 | otherwise = Just m
    where
      r = length rows
      c = length $ rows ! 0
      m = unsafePerformIO $
          withNewFMPQMat_ r c $ const $ \mptr ->
              forM_ (indexed rows) $ \(i, row) ->
              forM_ (indexed row) $ \(j, e) ->
              withFMPQ e $ const $ \eptr ->
              flip fmpq_set eptr =<< fmpq_mat_entryref mptr (fromIntegral i) (fromIntegral j)


-- Get the submatrix at (i,j) of size (r,c).  If necessary r and c are corrected.
submatrix :: Int -> Int -> Int -> Int -> FMPQMat -> FMPQMat
submatrix i j r c m = unsafePerformIO $
                      withNewFMPQMat_ r' c' $ const $ \cptr ->
                      withFMPQMat_ m $ const $ \aptr ->
                          forM_ (zip [0..r'-1] [i..i+r'-1]) $ \(ixc,ixa) ->
                          forM_ (zip [0..c'-1] [j..j+c'-1]) $ \(jxc,jxa) -> do
-- todo: in FLINT implement matrix windows and use them to copy the block
                                ce <- fmpq_mat_entryref cptr (fromIntegral ixc) (fromIntegral jxc)
                                ae <- fmpq_mat_entryref aptr (fromIntegral ixa) (fromIntegral jxa)
                                fmpq_set ce ae
              where
                r' = max 0 $ min r $ (flintMatR m) - i
                c' = max 0 $ min c $ (flintMatC m) - j

-- submatrixSM :: Int -> Int -> Int -> Int -> FMPQSubmat -> FMPQSubmat
-- submatrixSM i j r c (FMPQSubmat im jm rm cm m) = FMPQSubmat (im+i) (jm+j) r' c' m
--     where
--       r' = min r $ rm-im
--       c' = min c $ cm-jm

-- Cut a matrix a given rows and columns.
blockDecomposition :: [Int] -> [Int] -> FMPQMat -> [[FMPQMat]]
blockDecomposition is js m = 
    flip map (zip (0:is) (is++[flintMatR m])) $ \(i,inx) ->
    flip map (zip (0:js) (js++[flintMatC m])) $ \(j,jnx) ->
        submatrix i j (inx-i) (jnx-j) m

-- blockDecompositionSM :: [Int] -> [Int] -> FMPQSubmat -> [[FMPQSubmat]]
-- blockDecompositionSM is js m = 
--     flip map (zip (0:is) (is++[flintMatR m])) $ \(i,inx) ->
--     flip map (zip (0:js) (js++[flintMatC m])) $ \(j,jnx) ->
--         submatrixSM i j (inx-i) (jnx-j) m

-- block decomposition with prescribed position and size
blockDecompositionPosSize :: [(Int,Int)] -> [(Int,Int)] -> FMPQMat -> [[FMPQMat]]
blockDecompositionPosSize rpss cpss m =
    flip map rpss $ \(i,r) ->
    flip map cpss $ \(j,c) ->
        submatrix i j r c m

-- A submatrix starting at i j of size r c
-- type FMPQSubmat = (Int, Int, Int, Int, FMPQMat)

-- A matrix of given size (r,c) that is zero initialized and then
-- filled by the given submatrices
-- fromSubmatrices :: Int -> Int -> [(Int, Int, FMPQSubmat)] -> FMPQMat
-- -- todo: order by rows to address how flint handles memory
-- fromSubmatrices r c ms = unsafePerformIO $
--                          withNewFMPQMat_ r c $ const $ \cptr ->
--                          fmpq_mat_zero cptr
--                          forM_ ms $ \(ic,jc,(ia,ja,ra,ca,a)) ->
--                              withFMPQMat_ a $ const $ \aptr ->
--                              setWindow cptr ic jc
--                                        aptr ia ja ra ca

-- fromBlocks :: Int -> Int -> [FMPQSubmat] -> FMPQMat
-- fromBlocks = undefined
