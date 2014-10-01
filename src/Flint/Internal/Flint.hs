{-# LANGUAGE
    BangPatterns
  , TypeFamilies
  , FlexibleContexts
  , FlexibleInstances
  #-}

module Flint.Internal.Flint
    ( Flint(..)

    , lift0Flint
    , lift0Flint_
    , liftFlint
    , liftFlint_
    , lift2FlintWithType
    , lift2Flint
    , lift2Flint_

    -- , trivialFlintType0
    -- , trivialFlintType
    -- , trivialFlintType2
    )
where

import Foreign.Ptr(Ptr)
import Control.Applicative ((<$>))
import Control.Monad (void)
import Data.Composition

import System.IO.Unsafe (unsafePerformIO)


class Flint a where
    type CFlint a :: *
    type FlintType a :: *
    type CFlintType a :: *

    flintType :: a -> FlintType a

    newFlint :: FlintType a -> IO a

    withFlint :: a -> (Ptr (CFlintType a) -> Ptr (CFlint a) -> IO b) -> IO (a, b)

    withFlint_ :: a -> (Ptr (CFlintType a) -> Ptr (CFlint a) -> IO b) -> IO a
    withFlint_ f a = fst <$> withFlint f a

    withNewFlint :: FlintType a -> (Ptr (CFlintType a) -> Ptr (CFlint a) -> IO b) -> IO (a, b)
    withNewFlint t f = flip withFlint f =<< newFlint t

    withNewFlint_ :: FlintType a -> (Ptr (CFlintType a) -> Ptr (CFlint a) -> IO b) -> IO a
    withNewFlint_ t f = fst <$> withNewFlint t f


lift0Flint :: Flint a =>
              (Ptr (CFlintType a) -> Ptr (CFlint a) -> IO r) -> a -> IO r
lift0Flint f (!a) = fmap snd $ withFlint a f

lift0Flint_ :: Flint a =>
               (Ptr (CFlintType a) -> Ptr (CFlint a) -> IO r) -> a -> IO ()
lift0Flint_ = compose2 void lift0Flint

liftFlint :: ( Flint c, Flint a
             , FlintType c ~ FlintType a ) =>
             (Ptr (CFlintType c) -> Ptr (CFlint c) -> Ptr (CFlint a) -> IO r) ->
             a -> (c, IO r)
liftFlint f (!a) = (unsafePerformIO $ fst <$> cr, snd <$> cr)
    where
      cr = withNewFlint (flintType a) $ \ctype cptr -> fmap snd $
           withFlint a $ \_ aptr ->
           f ctype cptr aptr

liftFlint_ :: ( Flint c, Flint a
              , FlintType c ~ FlintType a ) =>
              (Ptr (CFlintType c) -> Ptr (CFlint c) -> Ptr (CFlint a) -> IO r) ->
              a -> c
liftFlint_ = compose2 fst liftFlint

lift2FlintWithType :: ( Flint c, Flint a, Flint b) =>
                      FlintType c ->
                      (Ptr (CFlintType c) ->  Ptr (CFlint c) -> Ptr (CFlint a) -> Ptr (CFlint b) -> IO r) ->
                      a -> b -> (c, IO r)
lift2FlintWithType t f (!a) (!b) = (unsafePerformIO $ fst <$> cr, snd <$> cr)
    where
      cr = withNewFlint t $ \ctype cptr -> fmap snd $
           withFlint a $ \_ aptr -> fmap snd $
           withFlint b $ \_ bptr ->
           f ctype cptr aptr bptr

lift2Flint :: ( Flint c, Flint a, Flint b
              , FlintType c ~ FlintType a, FlintType c ~ FlintType b ) =>
              (Ptr (CFlintType c) -> Ptr (CFlint c) -> Ptr (CFlint a) -> Ptr (CFlint b) -> IO r) ->
              a -> b -> (c, IO r)
lift2Flint f a = lift2FlintWithType (flintType a) f a

lift2Flint_ :: ( Flint c, Flint a, Flint b
               , FlintType c ~ FlintType a, FlintType c ~ FlintType b ) =>
               (Ptr (CFlintType c) -> Ptr (CFlint c) -> Ptr (CFlint a) -> Ptr (CFlint b) -> IO r) ->
               a -> b -> c
lift2Flint_ = compose3 fst lift2Flint
