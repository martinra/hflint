module HFlint.FMPQMat.Base
where

import Control.DeepSeq ( NFData(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPQ
import HFlint.FMPQ.FFI
import HFlint.FMPQMat.FFI


nmbRows :: FMPQMat -> Int
nmbRows m = fromIntegral $ unsafePerformIO $ snd <$>
  withFMPQMat m fmpq_mat_nrows

nmbCols :: FMPQMat -> Int
nmbCols m = fromIntegral $ unsafePerformIO $ snd <$>
  withFMPQMat m fmpq_mat_ncols

(!) :: FMPQMat -> (Int,Int) -> FMPQ 
m ! (ix,jx) = unsafePerformIO $
  withNewFMPQ_ $ \aptr ->
  withFMPQMat m $ \mptr -> do
    eptr <- fmpq_mat_entry mptr ix' jx'
    fmpq_set aptr eptr
  where
    ix' = fromIntegral ix
    jx' = fromIntegral jx

(!?) :: FMPQMat -> (Int,Int) -> Maybe FMPQ
m !? (ix,jx)
  | ix >= nrs || jx >= ncs = Nothing
  | otherwise              = Just $ unsafePerformIO $
      withNewFMPQ_ $ \aptr ->
      withFMPQMat m $ \mptr -> do
        eptr <- fmpq_mat_entry mptr ix' jx'
        fmpq_set aptr eptr
  where
    nrs = nmbRows m
    ncs = nmbCols m
    ix' = fromIntegral ix
    jx' = fromIntegral jx


fromVectors :: Vector (Vector FMPQ) -> FMPQMat
fromVectors rs
  | V.any ((/=ncs) . V.length) rs =
    error "FMPQMat.fromVectors unequal number of columns"
  | otherwise = unsafePerformIO $
      withNewFMPQMat_ nrs ncs $ \mptr ->
        V.forM_ (V.enumFromN 0 nrs) $ \ix ->
        V.forM_ (V.enumFromN 0 ncs) $ \jx -> do
          let ix' = fromIntegral ix
          let jx' = fromIntegral jx
          eptr <- fmpq_mat_entry mptr ix' jx'
          withFMPQ_ (rs V.! ix V.! jx) $ fmpq_set eptr 
  where
    nrs = V.length rs
    ncs | nrs == 0  = 0
        | otherwise = V.length $ V.head rs

toVectors :: FMPQMat -> Vector (Vector FMPQ)
toVectors m = unsafePerformIO $ fmap snd $
  withFMPQMat m $ \mptr ->
  V.generateM nrs $ \ix ->
  V.generateM ncs $ \jx ->
    withNewFMPQ_ $ \aptr -> do
      let ix' = fromIntegral ix
      let jx' = fromIntegral jx
      eptr <- fmpq_mat_entry mptr ix' jx'
      fmpq_set aptr eptr
  where
    nrs = nmbRows m
    ncs = nmbCols m


instance NFData FMPQMat where
  rnf (FMPQMat m) = seq m ()


rref :: FMPQMat -> FMPQMat
rref m = unsafePerformIO $
  withNewFMPQMat_ nrs ncs $ \rrptr ->
  withFMPQMat m $ \mmat ->
    fmpq_mat_rref rrptr mmat
  where
    nrs = nmbRows m
    ncs = nmbCols m
