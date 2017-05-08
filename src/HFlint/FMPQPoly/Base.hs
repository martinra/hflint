module HFlint.FMPQPoly.Base
where

import Prelude ()
import HFlint.Utility.Prelude

import qualified Data.Vector as V
import Foreign.C.String ( peekCString
                        , withCString
                        )
import Foreign.Marshal ( free )

import HFlint.Internal.Lift

import HFlint.FMPQ
import HFlint.FMPZ
import HFlint.FMPZ.FFI
import HFlint.FMPQPoly.FFI
import HFlint.FMPZPoly ()
import HFlint.FMPZPoly.FFI


--------------------------------------------------------------------------------
-- Show, Eq, NFData
--------------------------------------------------------------------------------

instance Show FMPQPoly where
    show a = unsafePerformIO $
      withCString "T" $ \cvar -> do
      (_,cstr) <- withFMPQPoly a$ \aptr ->
                  fmpq_poly_get_str_pretty aptr cvar
      str <- peekCString cstr
      free cstr
      return str

instance Eq FMPQPoly where
  (==) = (1==) .: (lift2Flint0 fmpq_poly_equal)

instance NFData FMPQPoly where
  rnf _ = ()

--------------------------------------------------------------------------------
-- container
--------------------------------------------------------------------------------

type instance Element FMPQPoly = FMPQ 

instance MonoFunctor FMPQPoly where
  omap f a = unsafePerformIO $
    withNewFMPQPoly_ $ \bptr ->
    withFMPQPoly_ a $ \aptr -> do
    n <- fmpq_poly_length aptr
    fmpq_poly_realloc bptr n
    V.forM_ (V.enumFromN 0 (fromIntegral n)) $ \dx -> do
      c <- withNewFMPQ_ $ \cptr -> fmpq_poly_get_coeff_fmpq cptr aptr (fromIntegral dx)
      withFMPQ_ (f c) $ fmpq_poly_set_coeff_fmpq bptr (fromIntegral dx)

--------------------------------------------------------------------------------
-- conversion
--------------------------------------------------------------------------------

fromFMPZ :: FMPZ -> FMPQPoly
fromFMPZ a = unsafePerformIO $
  withNewFMPQPoly_ $ \bptr ->
  withFMPZ_ a      $ \aptr ->
  fmpq_poly_set_fmpz bptr aptr

fromFMPQ :: FMPQ -> FMPQPoly
fromFMPQ a = unsafePerformIO $
  withNewFMPQPoly_ $ \bptr ->
  withFMPQ_ a      $ \aptr ->
  fmpq_poly_set_fmpq bptr aptr

fromVector :: Vector FMPQ -> FMPQPoly
fromVector as = unsafePerformIO $
  withNewFMPQPoly_ $ \bptr -> do
  fmpq_poly_realloc bptr (fromIntegral $ V.length as)
  sequence_ $ flip V.imap as $ \ix a ->
     withFMPQ_ a $ \aptr ->
     fmpq_poly_set_coeff_fmpq bptr (fromIntegral ix) aptr 

toVector :: FMPQPoly -> Vector FMPQ
toVector a = unsafePerformIO $ fmap snd $
  withFMPQPoly a $ \aptr -> do
  deg <- fromIntegral <$> fmpq_poly_degree aptr
  V.generateM (deg+1) $ \ix ->
    withNewFMPQ_ $ \bptr ->
    fmpq_poly_get_coeff_fmpq bptr aptr (fromIntegral ix)

fromList :: [FMPQ] -> FMPQPoly
fromList = fromVector . V.fromList

toList :: FMPQPoly -> [FMPQ]
toList = V.toList . toVector

fromRationals :: [Rational] -> FMPQPoly
fromRationals = fromVector . V.map fromRational . V.fromList

toRationals :: FMPQPoly -> [Rational]
toRationals = V.toList . V.map toRational . toVector


fromFMPZPoly :: FMPZPoly -> FMPQPoly
fromFMPZPoly = liftFlint_ fmpq_poly_set_fmpz_poly

toFMPZPoly :: FMPQPoly -> (FMPZ, FMPZPoly)
toFMPZPoly a = (den a, num a)
  where
  num :: FMPQPoly -> FMPZPoly
  num = liftFlint_
        fmpq_poly_get_numerator
  den :: FMPQPoly -> FMPZ
  den = liftFlint_ $ \denptr aptr ->
        fmpz_set denptr =<< fmpq_poly_denref aptr

--------------------------------------------------------------------------------
-- composition
--------------------------------------------------------------------------------

compose :: FMPQPoly -> FMPQPoly -> FMPQPoly
compose = lift2Flint_ fmpq_poly_compose
