{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module HFlint.NF.Base
where

import Control.DeepSeq ( NFData(..) )
import Data.Composition ( (.:) )
import qualified Data.Vector as V
import Data.Vector ( Vector )
import Foreign.C.String ( peekCString
                        , withCString
                        )
import Foreign.Marshal ( free )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPQ
import HFlint.FMPQPoly
  ( FMPQPoly
  , withFMPQPoly, withNewFMPQPoly_ )
import qualified HFlint.FMPQPoly as FMPQPoly
import HFlint.Internal.LiftCtx
import HFlint.NF.Context
import HFlint.NF.FFI


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
    deg <- fromIntegral <$> nf_degree_additional ctxptr
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
