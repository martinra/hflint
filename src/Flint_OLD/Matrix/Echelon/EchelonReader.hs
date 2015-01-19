module HFlint.Matrix.Echelon.EchelonReader
  ( EchelonReader
  )
where

import Control.Monad.Reader ( Reader )

import HFlint.Matrix.Echelon.EchelonParameters ( EchelonParameters )


newtype EchelonReader = Reader EchelonParameters
