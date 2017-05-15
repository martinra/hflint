{-# LANGUAGE TemplateHaskell #-}

module HFlint.NMod.Vector
where

import Data.Vector.Unboxed.Deriving ( derivingUnbox )
import Data.Word ( Word32 )
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable

import HFlint.NMod.FFI

derivingUnbox "NMod"
    [t| forall ctx .  NMod ctx -> Word32 |]
    [| fromIntegral . unNMod |]
    [| NMod . fromIntegral |]
