module HFlint.Internal.TH
where

import Control.Monad ( replicateM )
import Language.Haskell.TH


constTH :: Int -> ExpQ
constTH n = do
            x <- newName "x"
            ys <- replicateM n wildP
            return $ LamE (VarP x:ys) (VarE x)
