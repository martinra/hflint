{-# LANGUAGE
    FlexibleContexts
  , ScopedTypeVariables
  #-}

module HFlint.NMod.Reduction
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Proxy ( Proxy(..) )
import Data.Reflection ( reflect )

import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPQ.FFI
import HFlint.FMPZ.FFI

import HFlint.NMod.Context
import HFlint.NMod.FFI


{-# INLINE reduceFMPZ #-}
reduceFMPZ
  :: forall ctx .
     ReifiesNModContext ctx
  => FMPZ -> NMod ctx
reduceFMPZ a = unsafePerformIO $
  fmap snd $ withFMPZ a $ \aptr -> do
    p <- nmod_n $ reflect (Proxy :: Proxy ctx)
    NMod <$> fmpz_fdiv_ui aptr p

{-# INLINE reduceFMPQMay #-}
reduceFMPQMay
  :: forall ctx .
     ReifiesNModContext ctx
  => FMPQ -> Maybe (NMod ctx)
reduceFMPQMay a = unsafePerformIO $
  fmap snd $ withFMPQ a $ \aptr -> do
    p <- nmod_n $ reflect (Proxy :: Proxy ctx)
    numptr <- fmpq_numref aptr
    denptr <- fmpq_denref aptr
    num <- fmpz_fdiv_ui numptr p
    den <- fmpz_fdiv_ui denptr p
    if den == 0
      then return Nothing
      else Just <$> NMod <$> (
             nmod_div num den $ reflect (Proxy :: Proxy ctx)
             )
