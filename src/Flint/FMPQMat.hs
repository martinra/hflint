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

    , echelonForm
    )
where


import Flint.FMPQMat.FFI
import Flint.FMPQMat.Internal
import Flint.FMPQMat.Basic
import Flint.Echelon


