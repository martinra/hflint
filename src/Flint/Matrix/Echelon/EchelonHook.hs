module Flint.Matrix.Echelon.EchelonHook
where

import Flint.Matrix.Internal
import Flint.Matrix.Basic

import Flint.Algebra

import Flint.FMPQMat.FFI
import Flint.FMPQMat.Internal

import Prelude hiding (foldl)

import Data.Foldable (foldl)
import Control.Monad
import Data.Sequence (Seq, viewl, ViewL(..))
import Foreign.Ptr (Ptr)



data EchelonHook = EchelonHook (Seq Matrix) [Matrix]


hookTLMatrix :: a -> (Matrix -> a) -> EchelonHook -> a
hookTLMatrix def f (EchelonHook hl _) =
    case viewl hl of
      m:<_ -> f m
      _    -> def

hookCOffset :: EchelonHook -> Int
hookCOffset = hookTLMatrix 0 ncols

hookROffset :: EchelonHook -> Int
hookROffset = hookTLMatrix 0 nrows


mergeEchelonHooks :: [EchelonHook] -> Matrix
mergeEchelonHooks [] = zeroMatrix 0 0 QQ
mergeEchelonHooks (h:hs) = withNewMatrixCoordinates r c alg f
    where
      (r,c,f) = mergeEchelonHooks' (h:hs)
      alg = hookTLMatrix QQ algebra h


mergeEchelonHooks' :: [EchelonHook] -> (Int, Int, [Ptr CFMPQMat] -> IO ())
mergeEchelonHooks' = foldl step (0,0,const $ return ())

    where
      step (r,c,f) h = ( r + hookROffset h, c + hookCOffset h
                       , extendFByHook r c f h )

extendFByHook :: Int -> Int -> ([Ptr CFMPQMat] -> IO ()) -> EchelonHook ->
                 ([Ptr CFMPQMat] -> IO ())
extendFByHook r c f h@(EchelonHook hl ht) = f''
    where
      (_,_,f') = foldl stepL (r,c,f) hl
      (_,_,f'') = foldl stepT (r,c + hookCOffset h,f') ht

      stepL (r',c',g) m = ( r' + nrows m, c'
                          , extendFByBlock r' c' g m )
      stepT (r',c',g) m = ( r', c' + ncols m
                          , extendFByBlock r' c' g m )

extendFByBlock :: Int -> Int -> ([Ptr CFMPQMat] -> IO ()) -> Matrix ->
                  [Ptr CFMPQMat] -> IO ()
extendFByBlock r c f m aptrs = f aptrs >>
                               zipWithM_ set aptrs mcs 
    where
      set aptr mc = withFMPQMat_ mc $ const $ \mcptr ->
                    setWindow aptr r' c' mcptr 0 0 rm cm
      mcs = coordinates m
      r' = fromIntegral $ r
      c' = fromIntegral $ c
      rm = fromIntegral $ nrows m
      cm = fromIntegral $ ncols m
