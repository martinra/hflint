module HFlint.NMod.Base
where

import Control.DeepSeq ( NFData(..) )

import HFlint.NMod.FFI


instance Show NMod where
  show (NMod a) = show a

instance Eq NMod where
  {-# INLINE (==) #-}
  (NMod a) == (NMod a') = a == a'

instance Ord NMod where
  {-# INLINE compare #-}
  compare (NMod a) (NMod a') = compare a a'


instance NFData NMod where
  rnf (NMod a) = seq (rnf a) ()

