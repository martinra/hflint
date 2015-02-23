{-# LANGUAGE
    TemplateHaskell
  #-}

module HFlint.Internal.Const
where

import Control.Monad ( forM )
import Language.Haskell.TH

import HFlint.Internal.TH


forM [2..10] $ \n -> do
  let constF = mkName ("const"++show n)
  funD constF $ (:[]) $ clause [] (normalB $ constTH n) []
