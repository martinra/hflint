module Flint.FMPZ.Arithmetic
where


import Flint.Internal.Flint

import Flint.FMPZ.FFI
import Flint.FMPZ.Internal

import Foreign.C.Types (CULong(..))

import System.IO.Unsafe (unsafePerformIO)


instance Num FMPZ where
    -- todo : speed this up
    fromInteger a | a < 0 = negate (fromInteger (negate a))
                  | a == 0 = unsafePerformIO $ withNewFMPZ_ $ \_ cptr -> fmpz_zero cptr
                  | otherwise = unsafePerformIO $
                                withNewFMPZ_ $ const $ \cptr -> do
                                  fmpz_set_ui cptr $ head limbs
                                  flip mapM_ (tail limbs) $ \l -> do
                                    fmpz_mul_ui cptr cptr limbSize
                                    fmpz_add_ui cptr cptr l
                  where
                    limbSize = 1 + (div (maxBound :: CULong) 2)
                    limbs = map fromIntegral $
                            reverse $ map snd $ takeWhile (not . isZeroLimb) $
                            tail $ iterate nextLimb (a,0)
                    nextLimb = flip divMod (fromIntegral limbSize) . fst
                    isZeroLimb (b,l) = b==0 && l==0
    (+) = lift2Flint_ $ const fmpz_add
    (-) = lift2Flint_ $ const fmpz_sub
    (*) = lift2Flint_ $ const fmpz_mul
    abs = liftFlint_ $ const fmpz_abs
    signum = fromInteger . fromIntegral . unsafePerformIO .
             lift0Flint (const fmpz_sgn)
    negate = liftFlint_ $ const fmpz_neg
