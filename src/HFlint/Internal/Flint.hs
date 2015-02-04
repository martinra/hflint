{-# LANGUAGE
    BangPatterns
  , TypeFamilies
  , FlexibleContexts
  , FlexibleInstances
  , ScopedTypeVariables
  #-}

module HFlint.Internal.Flint
  ( Flint(..)

  , liftFlint0
  , lift2Flint0

  , liftFlintWithType
  , liftFlint
  , liftFlint_

  , lift2FlintWithType
  , lift2Flint
  , lift2Flint_
  , lift2Flint'

  , lift2Flint2WithType
  , lift2Flint2
  , lift2Flint2_
  , lift2Flint2'
  )
where

import Foreign.Ptr ( Ptr )
import Control.Applicative ( (<$>) )
import Control.Monad ( void )
import Data.Composition
import System.IO.Unsafe ( unsafePerformIO )


class Flint a where
    type CFlint a :: *
    type FlintType a :: *
    type CFlintType a :: *

    flintType :: a -> FlintType a

    newFlint :: FlintType a -> IO a

    withFlint :: a
              -> (Ptr (CFlintType a) -> Ptr (CFlint a) -> IO b)
              -> IO (a, b)

    withFlint_ :: a
               -> (Ptr (CFlintType a) -> Ptr (CFlint a) -> IO b)
               -> IO a
    withFlint_ f a = fst <$> withFlint f a

    withNewFlint :: FlintType a
                 -> (Ptr (CFlintType a) -> Ptr (CFlint a) -> IO b)
                 -> IO (a, b)
    withNewFlint t f = flip withFlint f =<< newFlint t

    withNewFlint_ :: FlintType a
                  -> (Ptr (CFlintType a) -> Ptr (CFlint a) -> IO b)
                  -> IO a
    withNewFlint_ t f = fst <$> withNewFlint t f

--------------------------------------------------
-- FMPZ -> ()
--------------------------------------------------

liftFlint0 :: Flint a
           => ( Ptr (CFlintType a) -> Ptr (CFlint a) -> IO r)
           -> a -> r 
liftFlint0 f (!a) = unsafePerformIO $ snd <$> withFlint a f

lift2Flint0 :: ( Flint a, Flint b )
            => (  Ptr (CFlintType a)
               -> Ptr (CFlint a) -> Ptr (CFlint b)
               -> IO r)
            -> a -> b -> r
lift2Flint0 f (!a) (!b) = unsafePerformIO $ fmap snd $
                          withFlint a $ \atype aptr -> fmap snd $
                          withFlint b $ \_ bptr ->
                          f atype aptr bptr

--------------------------------------------------
--- FMPZ -> FMPZ
--------------------------------------------------

liftFlintWithType :: ( Flint c, Flint a )
                  => FlintType c
                  -> (  Ptr (CFlintType c)
                     -> Ptr (CFlint c) -> Ptr (CFlint a)
                     -> IO r)
                  -> a -> (c, r)
liftFlintWithType t f (!a) =
  unsafePerformIO $
  withNewFlint t $ \ctype cptr -> fmap snd $
  withFlint a $ \_ aptr ->
  f ctype cptr aptr

liftFlint :: ( Flint c, Flint a
             , FlintType c ~ FlintType a )
          => (  Ptr (CFlintType c)
             -> Ptr (CFlint c) -> Ptr (CFlint a)
             -> IO r)
          -> a -> (c, r)
liftFlint f a = liftFlintWithType (flintType a) f a

liftFlint_ :: ( Flint c, Flint a
              , FlintType c ~ FlintType a )
           => (  Ptr (CFlintType c)
              -> Ptr (CFlint c)
              -> Ptr (CFlint a)
              -> IO r)
           -> a -> c
liftFlint_ = fst `compose2` liftFlint

--------------------------------------------------
--- FMPZ -> FMPZ -> FMPZ
--------------------------------------------------

lift2FlintWithType :: ( Flint c, Flint a, Flint b)
                   => FlintType c
                   -> (  Ptr (CFlintType c)
                      -> Ptr (CFlint c)
                      -> Ptr (CFlint a) -> Ptr (CFlint b)
                      -> IO r)
                   -> a -> b -> (c, r)
lift2FlintWithType t f (!a) (!b) =
  unsafePerformIO $
  withNewFlint t $ \ctype cptr -> fmap snd $
  withFlint a $ \_ aptr -> fmap snd $
  withFlint b $ \_ bptr ->
  f ctype cptr aptr bptr

lift2Flint :: ( Flint c, Flint a, Flint b
              , FlintType c ~ FlintType a )
           => (  Ptr (CFlintType c)
              -> Ptr (CFlint c)
              -> Ptr (CFlint a) -> Ptr (CFlint b)
              -> IO r)
           -> a -> b -> (c, r)
lift2Flint f a b = lift2FlintWithType (flintType a) f a b

lift2Flint_ :: ( Flint c, Flint a, Flint b
               , FlintType c ~ FlintType a, FlintType c ~ FlintType b )
            => (  Ptr (CFlintType c)
               -> Ptr (CFlint c)
               -> Ptr (CFlint a) -> Ptr (CFlint b)
               -> IO r)
            -> a -> b -> c
lift2Flint_ = fst `compose3` lift2Flint

lift2Flint' :: forall a b r .
               ( Flint a, Flint b )
            => (  Ptr (CFlintType a)
               -> Ptr (CFlint a)
               -> Ptr (CFlint a) -> Ptr (CFlint b)
               -> IO r)
            -> a -> b -> r
lift2Flint' f a b = snd cr
  where
  cr = lift2Flint f a b
  c = fst cr :: a

--------------------------------------------------
--- FMPZ -> FMPZ -> (FMPZ, FMPZ)
--------------------------------------------------

lift2Flint2WithType :: ( Flint c, Flint d, Flint a, Flint b )
                    => FlintType c -> FlintType d
                    -> (  Ptr (CFlintType c) -> Ptr (CFlintType d)
                       -> Ptr (CFlint c) -> Ptr (CFlint d)
                       -> Ptr (CFlint a) -> Ptr (CFlint b)
                       -> IO r )
                    -> a -> b -> ((c,d), r)
lift2Flint2WithType tc td f (!a) (!b) = ((c,d), r)
  where
  (c, (d,r)) = 
    unsafePerformIO $
    withNewFlint tc $ \ctype cptr ->
    withNewFlint td $ \dtype dptr -> fmap snd $
    withFlint a $ \_ aptr -> fmap snd $
    withFlint b $ \_ bptr ->
    f ctype dtype cptr dptr aptr bptr

lift2Flint2 :: ( Flint c, Flint d, Flint a, Flint b
               , FlintType c ~ FlintType a, FlintType d ~ FlintType a )
            => (  Ptr (CFlintType c) -> Ptr (CFlintType d)
               -> Ptr (CFlint c) -> Ptr (CFlint d)
               -> Ptr (CFlint a) -> Ptr (CFlint b)
               -> IO r)
            -> a -> b -> ((c,d), r)
lift2Flint2 f a b = lift2Flint2WithType (flintType a) (flintType a) f a b

lift2Flint2_ :: ( Flint c, Flint d, Flint a, Flint b
                , FlintType c ~ FlintType a, FlintType d ~ FlintType a )
             => (  Ptr (CFlintType c) -> Ptr (CFlintType d)
                -> Ptr (CFlint c) -> Ptr (CFlint d)
                -> Ptr (CFlint a) -> Ptr (CFlint b)
                -> IO r)
             -> a -> b -> (c,d)
lift2Flint2_ = fst `compose3` lift2Flint2 

lift2Flint2' :: forall a b r .
                ( Flint a, Flint b )
             => (  Ptr (CFlintType a) -> Ptr (CFlintType a)
                -> Ptr (CFlint a) -> Ptr (CFlint a)
                -> Ptr (CFlint a) -> Ptr (CFlint b)
                -> IO r)
             -> a -> b -> r
lift2Flint2' f a b = snd cr
  where
  cr = lift2Flint2 f a b
  c = fst cr :: (a,a)
