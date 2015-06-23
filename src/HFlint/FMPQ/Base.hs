module HFlint.FMPQ.Base
where

import Control.DeepSeq ( NFData(..) )
import Data.Composition ( (.:) )
import Foreign.C.String ( peekCString )
import Foreign.Ptr ( nullPtr )
import Foreign.Marshal ( free )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.Internal.Lift

import HFlint.FMPQ.FFI


instance Show FMPQ where
  show = toString 10

toString :: Int -> FMPQ -> String
toString base a = if '/' `elem` s then s
                  else s++"/1"
  where
  s = unsafePerformIO $ do
    (_,cstr) <- withFMPQ a $ fmpq_get_str nullPtr (fromIntegral base)
    str <- peekCString cstr
    free cstr
    return str

instance Eq FMPQ where
  {-# INLINE (==) #-}
  (==) = (1==) .: (lift2Flint0 fmpq_equal)

instance Ord FMPQ where
  {-# INLINE compare #-}
  compare = (toEnum . (+1) . fromInteger . toInteger) .:
            (lift2Flint0 fmpq_cmp)

instance NFData FMPQ where
  rnf _ = ()
