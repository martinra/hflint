module HFlint.FMPZPoly.Special
where

import Numeric.Natural
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPZPoly.FFI


cyclotomicPolynomial :: Int -> FMPZPoly
cyclotomicPolynomial n
  | n >= 0 = unsafePerformIO $
      withNewFMPZPoly_ $ \aptr -> do
      fmpz_poly_cyclotomic aptr (fromIntegral n)
  | otherwise = error "cyclotomicPolynomial: negative n"
