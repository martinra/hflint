module Flint.FMPQMat.Echelon
where

import Flint.Internal.Flint

import Flint.FMPQMat.FFI
import Flint.FMPQMat.Internal

import System.IO.Unsafe (unsafePerformIO)

echelonForm :: FMPQMat -> FMPQMat
echelonForm = liftFlint_ $ const fmpq_mat_rref
