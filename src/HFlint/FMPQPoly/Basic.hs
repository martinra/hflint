module HFlint.FMPQPoly.Basic
where

import Data.Composition ( (.:) )
import Foreign.C.String ( peekCString )
import Foreign.Ptr ( nullPtr )
import Foreign.Marshal ( free )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.Internal.Flint

import HFlint.FMPQPoly.Internal ()
import HFlint.FMPQPoly.FFI


instance Show FMPQPoly where
    show a = unsafePerformIO $ do
      (_,cstr) <- withFlint a $ const $ fmpq_poly_get_str
      str <- peekCString cstr
      free cstr
      return str

instance Eq FMPQPoly where
  (==) = (1==) .: (lift2Flint0 $ const fmpq_poly_equal)

instance Ord FMPQPoly where
  compare = (toEnum . (+1) . fromInteger . toInteger) .:
            (lift2Flint0 $ const fmpq_poly_cmp)
