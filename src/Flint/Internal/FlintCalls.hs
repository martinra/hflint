{-# LANGUAGE
    FunctionalDependencies
  , BangPatterns
  #-}

module Flint.Internal.FlintCalls
    ( Flint(..)
    , lift0Flint
    , lift0Flint_
    , liftFlint
    , liftFlint_
    , lift2Flint
    , lift2Flint_

    , FlintMat(..)
    , lift0FlintMat
    , lift0FlintMat_
    , liftFlintMat
    , liftFlintMat_
    , lift2FlintMat
    , lift2FlintMat_
    , lift2addFlintMat
    , lift2addFlintMat_
    , lift2mulFlintMat
    , lift2mulFlintMat_
    )
where

import Foreign.Ptr(Ptr)
import Control.Applicative ((<$>))
import Control.Monad (void)
import Data.Composition
import System.IO.Unsafe (unsafePerformIO)

class Flint a ca t | a -> ca t where
    flintType :: a -> t

    newFlint :: t -> IO a

    withFlint :: a -> (Ptr ca -> IO b) -> IO (a, b)

    withFlint_ :: a -> (Ptr ca -> IO b) -> IO a
    withFlint_ f a = fst <$> withFlint f a

    withNewFlint :: t -> (Ptr ca -> IO b) -> IO (a, b)
    withNewFlint t f = flip withFlint f =<< newFlint t

    withNewFlint_ :: t -> (Ptr ca -> IO b) -> IO a
    withNewFlint_ t f = fst <$> withNewFlint t f

lift0Flint :: (Flint a ca ta) =>
              (Ptr ca -> IO r) -> a -> IO r
lift0Flint f (!a) = fmap snd $ withFlint a f

lift0Flint_ :: (Flint a ca ta) =>
               (Ptr ca -> IO r) -> a -> IO ()
lift0Flint_ = compose2 void lift0Flint

liftFlint :: (Flint c cc t, Flint a ca t) =>
             (Ptr cc -> Ptr ca -> IO r) -> a -> (c, IO r)
liftFlint f (!a) = (unsafePerformIO $ fst <$> cr, snd <$> cr)
    where
      cr = withNewFlint (flintType a) $ \cptr -> fmap snd $
           withFlint a $ \aptr ->
           f cptr aptr

liftFlint_ :: (Flint c cc t, Flint a ca t) =>
              (Ptr cc -> Ptr ca -> IO r) -> a -> c
liftFlint_ = compose2 fst liftFlint

lift2Flint :: (Flint c cc t, Flint a ca t, Flint b cb t) =>
              (Ptr cc -> Ptr ca -> Ptr cb -> IO r) -> a -> b -> (c, IO r)
lift2Flint f (!a) (!b) = (unsafePerformIO $ fst <$> cr, snd <$> cr)
    where
      cr = withNewFlint (flintType a) $ \cptr -> fmap snd $
           withFlint a $ \aptr -> fmap snd $
           withFlint b $ \bptr ->
           f cptr aptr bptr

lift2Flint_ :: (Flint c cc t, Flint a ca t, Flint b cb t) =>
               (Ptr cc -> Ptr ca -> Ptr cb -> IO r) -> a -> b -> c
lift2Flint_ = compose3 fst lift2Flint



class FlintMat a ca t | a -> ca t where
    flintMatType :: a -> t
    flintMatR :: a -> Int
    flintMatC :: a -> Int

    newFlintMat :: t -> Int -> Int-> IO a

    withFlintMat :: a -> (Ptr ca -> IO b) -> IO (a, b)

    withFlintMat_ :: a -> (Ptr ca -> IO b) -> IO a
    withFlintMat_ f a = fst <$> withFlintMat f a

    withNewFlintMat :: t -> Int -> Int -> (Ptr ca -> IO b) -> IO (a, b)
    withNewFlintMat t r c f = flip withFlintMat f =<< newFlintMat t r c

    withNewFlintMat_ :: t -> Int -> Int -> (Ptr ca -> IO b) -> IO a
    withNewFlintMat_ t r c f = fst <$> withNewFlintMat t r c f

lift0FlintMat :: (FlintMat a ca ta) =>
                 (Ptr ca -> IO r) -> a -> IO r
lift0FlintMat f (!a) = fmap snd $ withFlintMat a f

lift0FlintMat_ :: (FlintMat a ca ta) =>
                  (Ptr ca -> IO r) -> a -> IO ()
lift0FlintMat_ f a = void $ lift0FlintMat f a 

liftFlintMat :: (FlintMat c cc t, FlintMat a ca t) =>
                (Ptr cc -> Ptr ca -> IO r) -> a -> (c, IO r)
liftFlintMat f (!a) = (unsafePerformIO $ fst <$> cr, snd <$> cr)
    where
      cr = withNewFlintMat t r c $ \cptr -> fmap snd $
           withFlintMat a $ \aptr ->
           f cptr aptr
      t = flintMatType a
      r = flintMatR a
      c = flintMatC a

liftFlintMat_ :: (FlintMat c cc t, FlintMat a ca t) =>
                 (Ptr cc -> Ptr ca -> IO r) -> a -> c
liftFlintMat_ f a = fst $ liftFlintMat f a

lift2FlintMat :: (FlintMat c cc t, FlintMat a ca t, FlintMat b cb t) =>
                 t -> Int -> Int ->
                 (Ptr cc -> Ptr ca -> Ptr cb -> IO r) -> a -> b -> (c, IO r)
lift2FlintMat t r c f (!a) (!b) = (unsafePerformIO $ fst <$> cr, snd <$> cr)
    where
      cr = withNewFlintMat t r c $ \cptr -> fmap snd $
           withFlintMat a $ \aptr -> fmap snd $
           withFlintMat b $ \bptr ->
           f cptr aptr bptr

lift2FlintMat_ :: (FlintMat c cc t, FlintMat a ca t, FlintMat b cb t) =>
                  t -> Int -> Int ->
                  (Ptr cc -> Ptr ca -> Ptr cb -> IO r) -> a -> b -> c
lift2FlintMat_ = compose6 fst lift2FlintMat

lift2addFlintMat :: (FlintMat c cc t, FlintMat a ca t, FlintMat b cb t) =>
                    (Ptr cc -> Ptr ca -> Ptr cb -> IO r) -> a -> b -> (c, IO r)
lift2addFlintMat f a b = lift2FlintMat t r c f a b
    where
      t = flintMatType a
      r = flintMatR a
      c = flintMatC a

lift2addFlintMat_ :: (FlintMat c cc t, FlintMat a ca t, FlintMat b cb t) =>
                     (Ptr cc -> Ptr ca -> Ptr cb -> IO r) -> a -> b -> c
lift2addFlintMat_ = compose3 fst lift2addFlintMat

lift2mulFlintMat :: (FlintMat c cc t, FlintMat a ca t, FlintMat b cb t) =>
                    (Ptr cc -> Ptr ca -> Ptr cb -> IO r) -> a -> b -> (c, IO r)
lift2mulFlintMat f a b = lift2FlintMat t r c f a b
    where
      t = flintMatType a
      r = flintMatR a
      c = flintMatC b

lift2mulFlintMat_ :: (FlintMat c cc t, FlintMat a ca t, FlintMat b cb t) =>
                     (Ptr cc -> Ptr ca -> Ptr cb -> IO r) -> a -> b -> c
lift2mulFlintMat_ = compose3 fst lift2mulFlintMat

