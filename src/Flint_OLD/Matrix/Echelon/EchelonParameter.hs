{-# LANGUAGE
    TemplateHaskell
  #-}

module HFlint.Matrix.Echelon.EchelonParameters
  ( EchelonParameters(..)
  , echelonRowCutOff
  , echelonColCutOff
  )
where

import Control.Lens.TH
import Data.Default ( Default )


data EchelonParameters = EchelonParameters
    { _echelonRowCutOff :: Int
    , _echelonColCutOff :: Int
    }
makeLenses ''EchelonParameters

instance Default EchelonParameters where
  def = EchelonParameter
          { _echelonRowCutOff = 64
          , _echelonColCutOff = 128
          }
