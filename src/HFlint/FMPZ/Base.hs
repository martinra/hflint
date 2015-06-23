module HFlint.FMPZ.Base
where

import Control.DeepSeq ( NFData(..) )
import Foreign.C.String ( peekCString )
import Foreign.Ptr ( nullPtr )
import Foreign.Marshal ( free )
import Data.Composition ( (.:) )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPZ.FFI
import HFlint.Internal.Lift


instance Show FMPZ where
    show = toString 10 

toString :: Int -> FMPZ -> String
toString base a = unsafePerformIO $ do
  (_,cstr) <- withFMPZ a $ fmpz_get_str nullPtr (fromIntegral base)
  str <- peekCString cstr
  free cstr
  return str

instance Eq FMPZ where
  (==) = (1==) .: (lift2Flint0 fmpz_equal)

instance Ord FMPZ where
  compare = (toEnum . (+1) . fromInteger . toInteger) .:
            (lift2Flint0  fmpz_cmp)

instance NFData FMPZ where
  rnf _ = ()
