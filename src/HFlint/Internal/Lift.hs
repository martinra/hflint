{-# LANGUAGE
    BangPatterns
  , FlexibleContexts
  , FlexibleInstances
  , ScopedTypeVariables
  , TypeFamilies
  #-}

module HFlint.Internal.Lift
  ( liftFlint0
  , lift2Flint0

  , lift0Flint
  , lift0Flint_

  , liftFlint
  , liftFlint_

  , lift2Flint
  , lift2Flint_
  , lift2Flint'

  , lift2Flint2
  , lift2Flint2_
  , lift2Flint2'

  , lift2Flint3
  , lift2Flint3_
  , lift2Flint3'
  )
where

import Data.Composition
import Foreign.Ptr ( Ptr )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.Internal.Flint


--------------------------------------------------
-- FMPZ -> ()
--------------------------------------------------

{-# INLINE liftFlint0 #-}
liftFlint0
  :: ( Flint a )
  => ( Ptr (CFlint a) -> IO r )
  -> a
  -> r
liftFlint0 f (!a) = unsafePerformIO $ snd <$> withFlint a f

{-# INLINE lift2Flint0 #-}
lift2Flint0
  :: ( Flint a, Flint b )
  => (    Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> a -> b
  -> r
lift2Flint0 f (!a) (!b) = unsafePerformIO $
  fmap snd $ withFlint a $ \aptr ->
  fmap snd $ withFlint b $ \bptr ->
  f aptr bptr

--------------------------------------------------
--- () -> FMPZ
--------------------------------------------------

{-# INLINE lift0Flint #-}
lift0Flint
  :: ( Flint c )
  => ( Ptr (CFlint c) -> IO r )
  -> (c, r)
lift0Flint f = unsafePerformIO $ withNewFlint f

{-# INLINE lift0Flint_ #-}
lift0Flint_
  :: ( Flint c )
  => ( Ptr (CFlint c) -> IO r )
  -> c
lift0Flint_ = fst . lift0Flint

--------------------------------------------------
--- FMPZ -> FMPZ
--------------------------------------------------

{-# INLINE liftFlint #-}
liftFlint
  :: ( Flint c, Flint a )
  => ( Ptr (CFlint c) -> Ptr (CFlint a) -> IO r )
  -> a
  -> (c, r)
liftFlint f (!a) = unsafePerformIO $
  fmap snd $ withFlint a  $ \aptr ->
             withNewFlint $ \cptr ->
  f cptr aptr

{-# INLINE liftFlint_ #-}
liftFlint_
  :: ( Flint c, Flint a )
  => ( Ptr (CFlint c) -> Ptr (CFlint a) -> IO r )
  -> a
  -> c
liftFlint_ = fst .: liftFlint

--------------------------------------------------
--- FMPZ -> FMPZ -> FMPZ
--------------------------------------------------

{-# INLINE lift2Flint #-}
lift2Flint
  :: ( Flint c, Flint a, Flint b )
  => (    Ptr (CFlint c)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> a -> b -> (c, r)
lift2Flint f (!a) (!b) = unsafePerformIO $
    fmap snd $ withFlint a  $ \aptr ->
    fmap snd $ withFlint b  $ \bptr ->
               withNewFlint $ \cptr ->
    f cptr aptr bptr

{-# INLINE lift2Flint_ #-}
lift2Flint_
  :: ( Flint c, Flint a, Flint b )
  => (    Ptr (CFlint c)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> a -> b
  -> c
lift2Flint_ = fst .:. lift2Flint

{-# INLINE lift2Flint' #-}
lift2Flint'
  :: forall a b r .
     ( Flint a, Flint b )
  => (    Ptr (CFlint a)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r)
  -> a -> b
  -> r
lift2Flint' f a b = snd cr
  where
  cr = lift2Flint f a b
  _ = fst cr :: a

--------------------------------------------------
--- FMPZ -> FMPZ -> (FMPZ, FMPZ)
--------------------------------------------------

{-# INLINE lift2Flint2 #-}
lift2Flint2
  :: ( Flint c, Flint d, Flint a, Flint b )
  => (    Ptr (CFlint c) -> Ptr (CFlint d)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> a -> b
  -> ((c,d), r)
lift2Flint2 f (!a) (!b) = unsafePerformIO $
  fmap snd $ withFlint a  $ \aptr ->
  fmap snd $ withFlint b  $ \bptr ->
  fmap cmb $ withNewFlint $ \cptr -> 
             withNewFlint $ \dptr -> 
  f cptr dptr aptr bptr
  where
   cmb (c, (d,r)) = ((c,d), r)

{-# INLINE lift2Flint2_ #-}
lift2Flint2_
  :: ( Flint c, Flint d, Flint a, Flint b )
  => (    Ptr (CFlint c) -> Ptr (CFlint d)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> a -> b
  -> (c,d)
lift2Flint2_ = fst .:. lift2Flint2

{-# INLINE lift2Flint2' #-}
lift2Flint2'
  :: forall a b r .
     ( Flint a, Flint b )
  => (    Ptr (CFlint a) -> Ptr (CFlint a)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> a -> b
  -> r
lift2Flint2' f a b = snd cdr
  where
  cdr = lift2Flint2 f a b
  _ = fst cdr :: (a,a)

--------------------------------------------------
--- FMPZ -> FMPZ -> (FMPZ, FMPZ, FMPZ)
--------------------------------------------------

{-# INLINE lift2Flint3 #-}
lift2Flint3
  :: ( Flint c, Flint d, Flint e, Flint a, Flint b )
  => (    Ptr (CFlint c) -> Ptr (CFlint d) -> Ptr (CFlint e)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> a -> b
  -> ((c,d,e), r)
lift2Flint3 f (!a) (!b) = unsafePerformIO $
    fmap snd $ withFlint a  $ \aptr ->
    fmap snd $ withFlint b  $ \bptr ->
    fmap cmb $ withNewFlint $ \cptr ->
               withNewFlint $ \dptr ->
               withNewFlint $ \eptr ->
    f cptr dptr eptr aptr bptr
  where
    cmb (c,(d,(e,r))) = ((c,d,e),r)

{-# INLINE lift2Flint3_ #-}
lift2Flint3_
  :: ( Flint c, Flint d, Flint e, Flint a, Flint b )
  => (    Ptr (CFlint c) -> Ptr (CFlint d) -> Ptr (CFlint e)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> a -> b
  -> (c,d,e)
lift2Flint3_ = fst .:. lift2Flint3

{-# INLINE lift2Flint3' #-}
lift2Flint3'
  :: forall a b r .
     ( Flint a, Flint b )
  => (    Ptr (CFlint a) -> Ptr (CFlint a) -> Ptr (CFlint a)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r)
  -> a -> b
  -> r
lift2Flint3' f a b = snd cder where
  cder = lift2Flint3 f a b
  _ = fst cder :: (a,a,a)
