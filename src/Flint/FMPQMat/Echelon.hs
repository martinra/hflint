module Flint.FMPQ.Echelon
where

echelonForm :: FMPQMat -> FMPQMat
echelonForm m = withNewFMPQMat r c $ const $ \nptr ->
                withFMPQMat $ const $ \mptr
                fmpq_mat_rref nptr mptr
