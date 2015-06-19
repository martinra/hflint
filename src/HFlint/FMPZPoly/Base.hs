module HFlint.FMPZPoly.Base
where

import Control.Applicative ( (<$>) )
import Control.DeepSeq ( NFData(..) )
import Data.Composition ( (.:) )
import qualified Data.Vector as V
import Data.Vector ( Vector )
import Foreign.C.String ( peekCString
                        , withCString
                        )
import Foreign.Marshal ( free )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.Internal.Flint

import HFlint.FMPZ ()
import HFlint.FMPZ.FFI
import HFlint.FMPZ.Internal ( withFMPZ_
                            , withNewFMPZ_
                            )
import HFlint.FMPZPoly.FFI
import HFlint.FMPZPoly.Internal ( withFMPZPoly
                                , withNewFMPZPoly_
                                )


instance Show FMPZPoly where
    show a = unsafePerformIO $ 
      withCString "T" $ \cvar -> do
      (_,cstr) <- withFMPZPoly a $ const $ \aptr ->
                  fmpz_poly_get_str_pretty aptr cvar
      str <- peekCString cstr
      free cstr
      return str

instance Eq FMPZPoly where
  (==) = (1==) .: (lift2Flint0 $ const fmpz_poly_equal)

instance NFData FMPZPoly where
  rnf _ = ()


fromVector :: Vector FMPZ -> FMPZPoly
fromVector as = unsafePerformIO $
  withNewFMPZPoly_ $ const $ \bptr -> do
  fmpz_poly_realloc bptr (fromIntegral $ V.length as)
  sequence_ $ flip V.imap as $ \ix a ->
     withFMPZ_ a $ const $ \aptr ->
     fmpz_poly_set_coeff_fmpz bptr (fromIntegral ix) aptr 

toVector :: FMPZPoly -> Vector FMPZ
toVector a = unsafePerformIO $ fmap snd $
  withFMPZPoly a $ const $ \aptr -> do
  deg <- fromIntegral <$> fmpz_poly_degree aptr
  V.generateM (deg+1) $ \ix ->
    withNewFMPZ_ $ const $ \bptr ->
    fmpz_poly_get_coeff_fmpz bptr aptr (fromIntegral ix)
  where

fromList :: [FMPZ] -> FMPZPoly
fromList = fromVector . V.fromList

toList :: FMPZPoly -> [FMPZ]
toList = V.toList . toVector

fromIntegers :: [Integer] -> FMPZPoly
fromIntegers = fromVector . V.map fromInteger . V.fromList

toIntegers :: FMPZPoly -> [Integer]
toIntegers = V.toList . V.map toInteger . toVector
