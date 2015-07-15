{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , InstanceSigs
  , MultiParamTypeClasses
  , RankNTypes
  , ScopedTypeVariables
  #-}

module HFlint.FMPZ.Reduction.FMPZCRT
where 

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Proxy ( Proxy(..) )
import Data.Reflection
import Math.Structure ( xgcd )

import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPZ.Algebra ()
import HFlint.FMPZ.Arithmetic ()
import HFlint.FMPZ.FFI
import HFlint.NMod ( ChineseRemainder(..)
                   , Modulus(..) )


newtype FMPZCRT ctx = FMPZCRT { unFMPZCRT :: FMPZ }

data FMPZCRTData = FMPZCRTData
  { fmpzModulus :: FMPZ
  , fmpzModulus' :: FMPZ
  , modinvFMPZModulus :: FMPZ
  }

type ReifiesFMPZCRTContext ctx' = Reifies ctx' FMPZCRTData


withFMPZCRTContext
  :: Modulus FMPZ -> Modulus FMPZ
  -> (forall ctx' . ReifiesFMPZCRTContext ctx' => Proxy ctx' -> b)
  -> b
withFMPZCRTContext (Modulus m) (Modulus m') = reify fmpzCRTData
  where
    fmpzCRTData = FMPZCRTData
      { fmpzModulus = m
      , fmpzModulus' = m'
      , modinvFMPZModulus = let (_,r,_) = xgcd m m' in r `mod` m'
      }

instance ReifiesFMPZCRTContext ctx' => ChineseRemainder FMPZ FMPZ (FMPZCRT ctx')
  where
    chineseRemainder a a' = FMPZCRT $ unsafePerformIO $
      withNewFMPZ_  $ \bptr    ->
      withFMPZ a    $ \aptr    ->
      withFMPZ a'   $ \a'ptr   ->
      withFMPZ m    $ \mptr    ->
      withFMPZ minv $ \minvptr ->
      withFMPZ m'   $ \m'ptr   -> do
        fmpz_fdiv_r bptr aptr m'ptr
    
        fmpz_sub bptr a'ptr bptr
        fmpz_fdiv_r bptr bptr m'ptr
    
        fmpz_mul bptr bptr minvptr
        fmpz_fdiv_r bptr bptr m'ptr
    
        fmpz_mul bptr bptr mptr
        fmpz_add bptr bptr aptr
      where
        fmpzCRTData = reflect (Proxy :: Proxy ctx')
        m = fmpzModulus fmpzCRTData
        m' = fmpzModulus' fmpzCRTData
        minv = modinvFMPZModulus fmpzCRTData
