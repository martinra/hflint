{-# LANGUAGE
    TypeFamilies
  #-}

module Flint.FMPQMat
    ( FMPQMat
    , withFMPQMat
    , withFMPQMat_
    , withNewFMPQMat
    , withNewFMPQMat_

    , fmpqMatFromVVector
    )
where


import Flint.FMPQMat.FFI
import Flint.FMPQMat.Internal
import Flint.FMPQMat.Basic


