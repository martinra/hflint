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
    )
where

import Foreign.Ptr(Ptr)
import Control.Applicative ((<$>))
import Control.Monad (void)
import System.IO.Unsafe (unsafePerformIO)

class Flint a ca | a -> ca where
    newFlint :: IO a

    withFlint :: a -> (Ptr ca -> IO b) -> IO (a, b)

    withFlint_ :: a -> (Ptr ca -> IO b) -> IO a
    withFlint_ f a = fst <$> withFlint f a

    withNewFlint :: (Ptr ca -> IO b) -> IO (a, b)
    withNewFlint f = flip withFlint f =<< newFlint

    withNewFlint_ :: (Ptr ca -> IO b) -> IO a
    withNewFlint_ f = fst <$> withNewFlint f

lift0Flint :: (Flint a ca) => (Ptr ca -> IO r) -> a -> IO r
lift0Flint f (!a) = fmap snd $ withFlint a f

lift0Flint_ :: (Flint a ca) => (Ptr ca -> IO r) -> a -> IO ()
lift0Flint_ f a = void $ lift0Flint f a 

liftFlint :: (Flint c cc, Flint a ca) => (Ptr cc -> Ptr ca -> IO r) -> a -> (c, IO r)
liftFlint f (!a) = (unsafePerformIO $ fst <$> cr, snd <$> cr)
    where
      cr = withNewFlint $ \cptr -> fmap snd $
           withFlint a $ \aptr ->
           f cptr aptr

liftFlint_ :: (Flint c cc, Flint a ca) => (Ptr cc -> Ptr ca -> IO r) -> a -> c
liftFlint_ f a = fst $ liftFlint f a

lift2Flint :: (Flint c cc, Flint a ca, Flint b cb) => (Ptr cc -> Ptr ca -> Ptr cb -> IO r) -> a -> b -> (c, IO r)
lift2Flint f (!a) (!b) = (unsafePerformIO $ fst <$> cr, snd <$> cr)
    where
      cr = withNewFlint $ \cptr -> fmap snd $
           withFlint a $ \aptr -> fmap snd $
           withFlint b $ \bptr ->
           f cptr aptr bptr

lift2Flint_ :: (Flint c cc, Flint a ca, Flint b cb) => (Ptr cc -> Ptr ca -> Ptr cb -> IO r) -> a -> b -> c
lift2Flint_ f a b = fst $ lift2Flint f a b
