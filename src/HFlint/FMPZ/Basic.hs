module HFlint.FMPZ.Basic
where

import Foreign.C.String ( peekCString )
import Foreign.Ptr ( nullPtr )
import Foreign.Marshal ( free )
import Data.Composition ( compose2 )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPZ.FFI
import HFlint.FMPZ.Internal ()
import HFlint.Internal.Flint


instance Show FMPZ where
    show = toString 10 

toString :: Int -> FMPZ -> String
toString base a = unsafePerformIO $ do
  (_,cstr) <- withFlint a $ const $ fmpz_get_str nullPtr (fromIntegral base)
  str <- peekCString cstr
  free cstr
  return str

instance Eq FMPZ where
  (==) = (1==) `compose2` (lift2Flint0 $ const fmpz_equal)

instance Ord FMPZ where
  compare = (toEnum . (+1) . fromInteger . toInteger) `compose2` (lift2Flint0 $ const fmpz_cmp)
