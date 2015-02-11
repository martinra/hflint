module HFlint.FMPQ.Basic
where

import Data.Ratio ( numerator
                  , denominator
                  )
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
  (_,cstr) <- withFlint a $ const $ fmpq_get_str nullPtr (fromIntegral base)
  str <- peekCString cstr
  free cstr
  return str

instance Eq FMPQ where
  (==) = (1==) . `compose2` (lift2Flint0 $ const fmpq_equal)

instance Ord FMPQ where
  compare = (toEnum . (+1) . fromInteger . toInteger) `compose2`
            (lift2Flint0 $ const fmpq_cmp)
