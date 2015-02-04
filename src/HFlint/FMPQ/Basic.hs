module HFlint.FMPQ.Basic
where

import Data.Ratio ( numerator
                  , denominator )
import Foreign.C.String ( peekCString )
import Foreign.Ptr ( nullPtr )
import Foreign.Marshal ( free )
import System.IO.Unsafe ( unsafePerformIO )

import Flint.Internal.Flint

import Flint.FMPQ.Internal
import Flint.FMPQ.FFI


instance Show FMPQ where
    show = toString 10

toString :: Int -> FMPQ -> String
toString base a = unsafePerformIO $ do
  cstr <- lift0Flint (const $ fmpq_get_str nullPtr (fromIntegral base)) a
  str <- peekCString cstr
  free cstr
  return str

instance Eq FMPQ where
  (==) = (1==) . unsafePerformIO .
        lift2Flint_ $ const fmpq_equal

instance Ord FMPQ where
  compare = toEnum . (+1) . unsafePerformIO .
            lift2Flint_ $ const fmpq_cmp

instance Enum FMPQ where
  succ = (+) $ fromInteger 1
  prec = (-) $ fromInteger 1
  toEnum = fromInteger

instance Num FMPQ where
  (+) = lift2Flint_ $ const fmpq_add
  (-) = lift2Flint_ $ const fmpq_sub
  (*) = lift2Flint_ $ const fmpq_mul

  abs = liftFlint_ $ const fmpq_abs
  signum = fromInteger . fromIntegral . unsafePerformIO .
           lift0Flint (const fmpq_sgn)
  negate = liftFlint_ $ const fmpq_neg

  fromInteger a = unsafePerformIO $
                  withNewFMPQ_ $ const $ \cptr ->
                  withFMPZ_ (fromInteger a) $ const $ \aptr ->
                  withNewFMPZ $ const $ \bptr -> do
                    fmpz_one bptr
                    fmpq_set_fmpz_frac cptr aptr bptr

instance Fractional FMPQ where
  fromRational a = unsafePerformIO $
                   withNewFMPQ_ $ const $ \cptr ->
                   withFMPZ (fromInteger num) $ const $ \aptr ->
                   withFMPZ (fromInteger den) $ const $ \bptr ->
                   fmpq_set_fmpz_frac cptr aptr bptr
    where
    num = numerator a
    den = denominator a

  (/) = lift2Flint_ $ const fmpq_div
  recip = liftFlint_ $ const fmpq_inv

instance Real FMPQ where
  toRational a = unsafePerformIO $ withFMPZ a $ const $ \aptr ->
                 let num = toInteger $ fromCFMPZ $ fmpq_numref a
                     den = toInteger $ fromCFMPZ $ fmpq_denref a
                 in return (num / den)
