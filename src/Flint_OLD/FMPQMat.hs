{-# LANGUAGE
    TypeFamilies
  #-}

module Flint.FMPQMat
    ( FMPQMat
    , withFMPQMat
    , withFMPQMat_
    , withNewFMPQMat
    , withNewFMPQMat_

    , toVVector
    , fromVVector

    , echelonForm
    )
where


import Flint.Internal.FlintMat

import Flint.FMPQMat.FFI
import Flint.FMPQMat.Internal
import Flint.FMPQMat.Basic
import Flint.FMPQMat.Echelon


