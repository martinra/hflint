module HFlint.FMPQPoly.Basic
where

import Control.Applicative ( (<$>) )
import Data.Composition ( (.:) )
import qualified Data.Vector as V
import Data.Vector ( Vector )
import Foreign.C.String ( peekCString )
import Foreign.Marshal ( free )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.Internal.Flint

import HFlint.FMPQ
import HFlint.FMPQ.FFI
import HFlint.FMPQ.Internal ( withFMPQ_
                            , withNewFMPQ_
                            )
import HFlint.FMPQPoly.FFI
import HFlint.FMPQPoly.Internal ( withFMPQPoly
                                , withNewFMPQPoly_
                                )


instance Show FMPQPoly where
    show a = unsafePerformIO $ do
      (_,cstr) <- withFlint a $ const $ fmpq_poly_get_str
      str <- peekCString cstr
      free cstr
      return str

instance Eq FMPQPoly where
  (==) = (1==) .: (lift2Flint0 $ const fmpq_poly_equal)


fromVector :: Vector FMPQ -> FMPQPoly
fromVector as = unsafePerformIO $
  withNewFMPQPoly_ $ const $ \bptr -> do
  sequence_ $ (flip V.imap) as $ \ix a ->
     withFMPQ_ a $ const $ \aptr ->
     fmpq_poly_set_coeff_fmpq bptr aptr (fromIntegral ix)

toVector :: FMPQPoly -> Vector FMPQ
toVector a = unsafePerformIO $ fmap snd $
  withFMPQPoly a $ const $ \aptr -> do
  deg <- fromIntegral <$> fmpq_poly_degree aptr
  V.generateM (deg+1) $ \ix ->
    withNewFMPQ_ $ const $ \bptr ->
    fmpq_poly_get_coeff_fmpq bptr aptr (fromIntegral ix)
  where

fromList :: [FMPQ] -> FMPQPoly
fromList = fromVector . V.fromList

toList :: FMPQPoly -> [FMPQ]
toList = V.toList . toVector

fromRationals :: [Rational] -> FMPQPoly
fromRationals = fromVector . V.map fromRational . V.fromList

toRationals :: FMPQPoly -> [Rational]
toRationals = V.toList . V.map toRational . toVector

