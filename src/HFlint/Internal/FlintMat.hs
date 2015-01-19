{-# LANGUAGE
    TypeFamilies
  , FlexibleContexts
  , FlexibleInstances
  #-}

module Flint.Internal.FlintMat
    ( FlintMat(..)
    , flintMatEntryType
    , flintMatR
    , flintMatC
    , toVVector

    , lift2addFlintMat
    , lift2addFlintMat_
    , lift2mulFlintMat
    , lift2mulFlintMat_
    )
where

import Flint.Internal.Flint

import Foreign.Ptr(Ptr)
import Data.Vector (Vector, generate)
import Data.Composition


class (Flint (FlintMatEntry a)) => FlintMat a where
    type FlintMatEntry a :: *

    flintMatEntry :: a -> Int -> Int -> FlintMatEntry a

flintMatEntryType :: ( Flint a, FlintMat a
                     , FlintType a ~ (FlintType (FlintMatEntry a), Int, Int) ) =>
                     a -> FlintType (FlintMatEntry a)
flintMatEntryType m = t
    where
      (t, _, _) = flintType m

flintMatR :: ( Flint a, FlintMat a
             , FlintType a ~ (FlintType (FlintMatEntry a), Int, Int) ) =>
             a -> Int
flintMatR m = r
    where
      (_, r, _) = flintType m

flintMatC :: ( Flint a, FlintMat a
             , FlintType a ~ (FlintType (FlintMatEntry a), Int, Int) ) =>
             a -> Int
flintMatC m = c
    where
      (_, _, c) = flintType m

toVVector ::  ( Flint a, FlintMat a
                      , FlintType a ~ (FlintType (FlintMatEntry a), Int, Int) ) =>
                      a -> Vector (Vector (FlintMatEntry a))
toVVector m = generate (flintMatR m) $ \i ->
                      generate (flintMatC m) $ \j ->
                      flintMatEntry m i j


-- only the lift2 functions have to be defined separately

lift2addFlintMat :: ( Flint c, Flint a, Flint b
                    , FlintMat c, FlintMat a, FlintMat b
                    , FlintType (FlintMatEntry c) ~ FlintType (FlintMatEntry a)
                    , FlintType (FlintMatEntry c) ~ FlintType (FlintMatEntry b)
                    , FlintType c ~ (FlintType (FlintMatEntry c), Int,Int)
                    , FlintType a ~ (FlintType (FlintMatEntry a), Int,Int)
                    , FlintType b ~ (FlintType (FlintMatEntry b), Int,Int)
                    , CFlintType c ~ CFlintType (FlintMatEntry c) ) =>
                    (Ptr (CFlintType (FlintMatEntry c)) -> Ptr (CFlint c) -> Ptr (CFlint a) -> Ptr (CFlint b) -> IO r) ->
                    a -> b -> (c, IO r)
lift2addFlintMat f a b = lift2FlintWithType (t,r,c) f a b
    where
      t = flintType $ flintMatEntry a 0 0
      r = flintMatR a
      c = flintMatC a

lift2addFlintMat_ :: ( Flint c, Flint a, Flint b
                    , FlintMat c, FlintMat a, FlintMat b
                    , FlintType (FlintMatEntry c) ~ FlintType (FlintMatEntry a)
                    , FlintType (FlintMatEntry c) ~ FlintType (FlintMatEntry b)
                    , FlintType c ~ (FlintType (FlintMatEntry c), Int,Int)
                    , FlintType a ~ (FlintType (FlintMatEntry a), Int,Int)
                    , FlintType b ~ (FlintType (FlintMatEntry b), Int,Int)
                    , CFlintType c ~ CFlintType (FlintMatEntry c) ) =>
                    (Ptr (CFlintType (FlintMatEntry c)) -> Ptr (CFlint c) -> Ptr (CFlint a) -> Ptr (CFlint b) -> IO r) ->
                    a -> b -> c
lift2addFlintMat_ = compose3 fst lift2addFlintMat

lift2mulFlintMat :: ( Flint c, Flint a, Flint b
                    , FlintMat c, FlintMat a, FlintMat b
                    , FlintType (FlintMatEntry c) ~ FlintType (FlintMatEntry a)
                    , FlintType (FlintMatEntry c) ~ FlintType (FlintMatEntry b)
                    , FlintType c ~ (FlintType (FlintMatEntry c), Int,Int)
                    , FlintType a ~ (FlintType (FlintMatEntry a), Int,Int)
                    , FlintType b ~ (FlintType (FlintMatEntry b), Int,Int)
                    , CFlintType c ~ CFlintType (FlintMatEntry c)  ) =>
                    (Ptr (CFlintType (FlintMatEntry c)) -> Ptr (CFlint c) -> Ptr (CFlint a) -> Ptr (CFlint b) -> IO r) ->
                    a -> b -> (c, IO r)
lift2mulFlintMat f a b = lift2FlintWithType (t,r,c) f a b
    where
      t = flintType $ flintMatEntry a 0 0
      r = flintMatR a
      c = flintMatC b

lift2mulFlintMat_ :: ( Flint c, Flint a, Flint b
                    , FlintMat c, FlintMat a, FlintMat b
                    , FlintType (FlintMatEntry c) ~ FlintType (FlintMatEntry a)
                    , FlintType (FlintMatEntry c) ~ FlintType (FlintMatEntry b)
                    , FlintType c ~ (FlintType (FlintMatEntry c), Int,Int)
                    , FlintType a ~ (FlintType (FlintMatEntry a), Int,Int)
                    , FlintType b ~ (FlintType (FlintMatEntry b), Int,Int)
                    , CFlintType c ~ CFlintType (FlintMatEntry c)  ) =>
                    (Ptr (CFlintType (FlintMatEntry c)) -> Ptr (CFlint c) -> Ptr (CFlint a) -> Ptr (CFlint b) -> IO r) ->
                    a -> b -> c
lift2mulFlintMat_ = compose3 fst lift2mulFlintMat
