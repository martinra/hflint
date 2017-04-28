module HFlint.FMPZPoly.Special
where

import Numeric.Natural
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPZPoly.FFI


cyclotomicPolynomial :: Natural -> FMPZPoly
cyclotomicPolynomial n = unsafePerformIO $
  withNewFMPZPoly_ $ \aptr -> do
  fmpz_poly_cyclotomic aptr (fromIntegral n)
