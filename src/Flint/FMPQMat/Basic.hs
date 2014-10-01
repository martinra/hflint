module Flint.FMPQMat.Basic
where


import Flint.FMPQ.FFI
import Flint.FMPQ.Internal

import Flint.FMPQMat.FFI
import Flint.FMPQMat.Internal

import Prelude hiding (length)

import Data.Vector (Vector, (!), forM_, indexed, length)

import System.IO.Unsafe (unsafePerformIO)


fmpqMatFromVVector :: Vector (Vector FMPQ) -> Maybe FMPQMat
fmpqMatFromVVector rows | length rows == 0 = Nothing
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
