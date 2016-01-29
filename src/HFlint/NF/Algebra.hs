{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  , UndecidableInstances
  #-}

module HFlint.NF.Algebra
where


import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
import qualified Prelude as P

import Math.Structure.Additive
import Math.Structure.Multiplicative
import Math.Structure.Ring

import HFlint.Internal.LiftCtx
import HFlint.NF.Arithmetic ()
import HFlint.NF.Context
import HFlint.NF.FFI


instance    ReifiesNFContext ctxProxy
         => AdditiveMagma (NF ctxProxy)
  where
  (+) = (P.+)
 
instance    ReifiesNFContext ctxProxy
         => Abelian (NF ctxProxy)

instance    ReifiesNFContext ctxProxy
         => AdditiveSemigroup (NF ctxProxy)

instance    ReifiesNFContext ctxProxy
         => AdditiveMonoid (NF ctxProxy)
  where
  zero = lift0FlintCtx_ nf_elem_zero

instance    ReifiesNFContext ctxProxy
         => DecidableZero (NF ctxProxy)
  where
  isZero = (/=0) . liftFlintCtx0 nf_elem_is_zero

instance    ReifiesNFContext ctxProxy
         => AdditiveGroup (NF ctxProxy)
  where
  negate = P.negate
  (-) = (P.-)


instance    ReifiesNFContext ctxProxy
         => MultiplicativeMagma (NF ctxProxy)
  where
  (*) = (P.*)

instance    ReifiesNFContext ctxProxy
         => Commutative (NF ctxProxy)

instance    ReifiesNFContext ctxProxy
         => MultiplicativeSemigroup (NF ctxProxy)

instance    ReifiesNFContext ctxProxy
         => MultiplicativeMonoid (NF ctxProxy)
  where
  one = lift0FlintCtx_ nf_elem_one

instance    ReifiesNFContext ctxProxy
         => DecidableOne (NF ctxProxy)
  where
  isOne = (/=0) . liftFlintCtx0 nf_elem_is_one


deriving instance ReifiesNFContext ctxProxy
  => MultiplicativeMagma (NonZero (NF ctxProxy))

instance    ReifiesNFContext ctxProxy
         => Commutative (NonZero (NF ctxProxy))

instance    ReifiesNFContext ctxProxy
         => MultiplicativeSemigroup (NonZero (NF ctxProxy))

instance    ReifiesNFContext ctxProxy
         => MultiplicativeMonoid (NonZero (NF ctxProxy))
  where
  one  = NonZero one

instance    ReifiesNFContext ctxProxy
         => DecidableOne (NonZero (NF ctxProxy))
  where
  isOne (NonZero a) = isOne a

instance    ReifiesNFContext ctxProxy
         => MultiplicativeGroup (NonZero (NF ctxProxy))
  where
  recip (NonZero a) = NonZero ( P.recip a )
  (NonZero a) / (NonZero b) = NonZero (a P./ b)


instance    ReifiesNFContext ctxProxy
         => Distributive (NF ctxProxy)

instance    ReifiesNFContext ctxProxy
         => Semiring (NF ctxProxy)

instance    ReifiesNFContext ctxProxy
         => Rng (NF ctxProxy)

instance    ReifiesNFContext ctxProxy
         => Rig (NF ctxProxy)

instance    ReifiesNFContext ctxProxy
         => Ring (NF ctxProxy)

instance    ReifiesNFContext ctxProxy
         => IntegralDomain (NF ctxProxy)

instance    ReifiesNFContext ctxProxy
         => DivisionRing (NF ctxProxy)

instance    ReifiesNFContext ctxProxy
         => Field (NF ctxProxy)
