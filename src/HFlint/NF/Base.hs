{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module HFlint.NF.Base
where

import Prelude ()
import HFlint.Utility.Prelude

import qualified Data.Vector as V
import Foreign.C.String ( peekCString
                        , withCString
                        )
import Foreign.Marshal ( free )

import HFlint.FMPQ
import HFlint.FMPQPoly ( FMPQPoly, withFMPQPoly, withNewFMPQPoly_ )
import HFlint.FMPZ
import HFlint.Internal.LiftCtx
import HFlint.NF.Context
import HFlint.NF.FFI
import qualified HFlint.FMPQPoly as FMPQPoly


--------------------------------------------------------------------------------
-- Show, Eq, NFData
--------------------------------------------------------------------------------

instance ReifiesNFContext ctxProxy => Show (NF ctxProxy) where
  show a = unsafePerformIO $ 
    withCString "T" $ \cvar -> do
    (_,cstr) <- withNF a $ \aptr ctxptr ->
                nf_elem_get_str_pretty aptr cvar ctxptr
    str <- peekCString cstr
    free cstr
    return str

instance ReifiesNFContext ctxProxy => Eq (NF ctxProxy) where
  (==) = (1==) .: lift2FlintCtx0 nf_elem_equal


instance ReifiesNFContext ctxProxy => NFData (NF ctxProxy) where
  rnf _ = ()

--------------------------------------------------------------------------------
-- conversion
--------------------------------------------------------------------------------

fromFMPZ
  :: ReifiesNFContext ctxProxy
  => FMPZ -> NF ctxProxy
fromFMPZ a = unsafePerformIO $
  withNewNF_                 $ \bptr ctxptr ->
  withFMPZ_ a                $ \aptr        ->
    nf_elem_set_fmpz bptr aptr ctxptr

fromFMPQ
  :: ReifiesNFContext ctxProxy
  => FMPQ -> NF ctxProxy
fromFMPQ a = unsafePerformIO $
  withNewNF_                 $ \bptr ctxptr ->
  withFMPQ_ a                $ \aptr        ->
    nf_elem_set_fmpq bptr aptr ctxptr

fromFMPQPoly
  :: ReifiesNFContext ctxProxy
  => FMPQPoly -> NF ctxProxy
fromFMPQPoly a = unsafePerformIO $
  withNewNF_     $ \bptr ctxptr ->
  withFMPQPoly a $ \aptr        ->
    nf_elem_set_fmpq_poly bptr aptr ctxptr

toFMPQPoly
  :: ReifiesNFContext ctxProxy
  => NF ctxProxy -> FMPQPoly
toFMPQPoly a = unsafePerformIO $
  withNewFMPQPoly_ $ \bptr        ->
  withNF a         $ \aptr ctxptr ->
    nf_elem_get_fmpq_poly bptr aptr ctxptr

fromVector
  :: ReifiesNFContext ctxProxy
  => Vector FMPQ -> NF ctxProxy
fromVector = fromFMPQPoly . FMPQPoly.fromVector

toVector
  :: ReifiesNFContext ctxProxy
  => NF ctxProxy -> Vector FMPQ
toVector a = unsafePerformIO $ fmap snd $
  withNF a $ \aptr ctxptr -> do
    deg <- fromIntegral <$> nf_degree ctxptr
    V.generateM (deg+1) $ \ix ->
      withNewFMPQ_ $ \bptr ->
      nf_elem_get_coeff_fmpq bptr aptr (fromIntegral ix) ctxptr

fromList
  :: ReifiesNFContext ctxProxy
  => [FMPQ] -> NF ctxProxy
fromList = fromVector . V.fromList

toList
  :: ReifiesNFContext ctxProxy
  => NF ctxProxy -> [FMPQ]
toList = V.toList . toVector

fromRationals
  :: ReifiesNFContext ctxProxy
  => [Rational] -> NF ctxProxy
fromRationals = fromVector . V.map fromRational . V.fromList

toRationals
  :: ReifiesNFContext ctxProxy
  => NF ctxProxy -> [Rational]
toRationals = V.toList . V.map toRational . toVector

--------------------------------------------------------------------------------
-- attributes
--------------------------------------------------------------------------------

degree :: ReifiesNFContext ctx => Proxy ctx -> Natural
degree proxy = unsafePerformIO $ fmap fromIntegral $ nf_degree $ reflect proxy

--------------------------------------------------------------------------------
-- creation
--------------------------------------------------------------------------------

gen :: ReifiesNFContext ctx => NF ctx
gen = lift0FlintCtx_ nf_elem_gen
