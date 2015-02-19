{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module HFlint.FMPZPoly.Factor
where

import Control.Arrow ( second )
import Control.Applicative ( (<$>) )
import qualified Data.Vector as V
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPZ
import HFlint.FMPZPoly.Internal
import HFlint.FMPZPoly.FFI
import HFlint.FMPZPolyFactor
import HFlint.FMPZPolyFactor.FFI

import HFlint.Internal.Factor


instance Factorizable FMPZPoly FMPZ where
  factor a = unsafePerformIO $ fmap snd $
             withNewFMPZPolyFactor $ \_ pfptr -> fmap snd $
             withFMPZPoly a $ \_ aptr ->do
               fmpz_poly_factor_zassenhaus pfptr aptr
               n <- fromIntegral <$> fmpz_poly_factor_number_factors pfptr
               u <- withNewFMPZ_ $ \_ uptr ->
                    fmpz_poly_factor_get_content uptr pfptr
               f <- if n==0 then return V.empty
                    else V.generateM n $ \i -> fmap (second fromIntegral) $
                         withNewFMPZPoly $ \_ fptr ->
                         fmpz_poly_factor_get_factor fptr pfptr (fromIntegral i)
               return $ Factored u f 
