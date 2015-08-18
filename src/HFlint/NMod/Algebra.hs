{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  , UndecidableInstances
  #-}

module HFlint.NMod.Algebra
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

import HFlint.Internal.Context
import HFlint.NMod.Arithmetic ()
import HFlint.NMod.FFI


instance    ReifiesFlintContext NModCtx ctxProxy
         => AdditiveMagma (NMod ctxProxy)
  where
  (+) = (P.+)
 
instance    ReifiesFlintContext NModCtx ctxProxy
         => Abelian (NMod ctxProxy)

instance    ReifiesFlintContext NModCtx ctxProxy
         => AdditiveSemigroup (NMod ctxProxy)

instance    ReifiesFlintContext NModCtx ctxProxy
         => AdditiveMonoid (NMod ctxProxy)
  where
  zero = NMod 0

instance    ReifiesFlintContext NModCtx ctxProxy
         => DecidableZero (NMod ctxProxy)
  where
  isZero = (==0) . unNMod

instance    ReifiesFlintContext NModCtx ctxProxy
         => AdditiveGroup (NMod ctxProxy)
  where
  negate = P.negate
  (-) = (P.-)


instance    ReifiesFlintContext NModCtx ctxProxy
         => MultiplicativeMagma (NMod ctxProxy)
  where
  (*) = (P.*)

instance    ReifiesFlintContext NModCtx ctxProxy
         => Commutative (NMod ctxProxy)

instance    ReifiesFlintContext NModCtx ctxProxy
         => MultiplicativeSemigroup (NMod ctxProxy)

instance    ReifiesFlintContext NModCtx ctxProxy
         => MultiplicativeMonoid (NMod ctxProxy)
  where
  one = NMod 1

instance    ReifiesFlintContext NModCtx ctxProxy
         => DecidableOne (NMod ctxProxy)
  where
  isOne = (==1) . unNMod


deriving instance ReifiesFlintContext NModCtx ctxProxy
  => MultiplicativeMagma (NonZero (NMod ctxProxy))

instance    ReifiesFlintContext NModCtx ctxProxy
         => Commutative (NonZero (NMod ctxProxy))

instance    ReifiesFlintContext NModCtx ctxProxy
         => MultiplicativeSemigroup (NonZero (NMod ctxProxy))

instance    ReifiesFlintContext NModCtx ctxProxy
         => MultiplicativeMonoid (NonZero (NMod ctxProxy))
  where
  one  = NonZero one

instance    ReifiesFlintContext NModCtx ctxProxy
         => DecidableOne (NonZero (NMod ctxProxy))
  where
  isOne (NonZero a) = isOne a

instance    ReifiesFlintContext NModCtx ctxProxy
         => MultiplicativeGroup (NonZero (NMod ctxProxy))
  where
  recip (NonZero a)= NonZero ( P.recip a )
  (NonZero a) / (NonZero b) = NonZero (a P./ b)


instance    ReifiesFlintContext NModCtx ctxProxy
         => Distributive (NMod ctxProxy)

instance    ReifiesFlintContext NModCtx ctxProxy
         => Semiring (NMod ctxProxy)

instance    ReifiesFlintContext NModCtx ctxProxy
         => Rng (NMod ctxProxy)

instance    ReifiesFlintContext NModCtx ctxProxy
         => Rig (NMod ctxProxy)

instance    ReifiesFlintContext NModCtx ctxProxy
         => Ring (NMod ctxProxy)

instance    ReifiesFlintContext NModCtx ctxProxy
         => IntegralDomain (NMod ctxProxy)

instance    ReifiesFlintContext NModCtx ctxProxy
         => DivisionRing (NMod ctxProxy)

instance    ReifiesFlintContext NModCtx ctxProxy
         => Field (NMod ctxProxy)
