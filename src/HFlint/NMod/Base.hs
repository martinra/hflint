module HFlint.NMod.Base
where

import Control.DeepSeq ( NFData(..) )

import HFlint.NMod.FFI


instance Show (NMod ctxProxy) where
  show (NMod a) = show a

instance Eq (NMod ctxProxy) where
  {-# INLINE (==) #-}
  (NMod a) == (NMod a') = a == a'

instance Ord (NMod ctxProxy) where
  {-# INLINE compare #-}
  compare (NMod a) (NMod a') = compare a a'


instance NFData (NMod ctxProxy) where
  rnf (NMod a) = seq a ()

