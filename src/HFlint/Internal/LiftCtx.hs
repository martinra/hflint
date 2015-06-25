{-# LANGUAGE
    BangPatterns
  , TypeFamilies
  , FlexibleContexts
  , FlexibleInstances
  , ScopedTypeVariables
  #-}

module HFlint.Internal.LiftCtx
  ( liftFlint0Ctx
  , lift2Flint0Ctx

  , lift0FlintCtx
  , lift0FlintCtx_

  , liftFlintCtx
  , liftFlintCtx_

  , lift2FlintCtx
  , lift2FlintCtx_
  , lift2FlintCtx'

  , lift2Flint2Ctx
  , lift2Flint2Ctx_
  , lift2Flint2Ctx'

  , lift2Flint3Ctx
  , lift2Flint3Ctx_
  , lift2Flint3Ctx'
  )
where

import Data.Composition
import Foreign.Ptr ( Ptr )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.Internal.Context
import HFlint.Internal.Flint
import HFlint.Internal.FlintWithContext


--------------------------------------------------
-- FMPZ -> ()
--------------------------------------------------

{-# INLINE liftFlint0Ctx #-}
liftFlint0Ctx
  :: ( FlintWithContext ctx a )
  => ( Ptr (CFlint a) -> Ptr (CFlintCtx ctx) -> IO r )
  -> a
  -> r
liftFlint0Ctx f (!a) = unsafePerformIO $ snd <$> withFlintCtx a f

{-# INLINE lift2Flint0Ctx #-}
lift2Flint0Ctx
  :: ( FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlint a) -> Ptr (CFlint b)
       -> Ptr (CFlintCtx ctx)
       -> IO r )
  -> a -> b
  -> r
lift2Flint0Ctx f (!a) (!b) = unsafePerformIO $
  fmap snd $ withFlint a    $ \aptr     ->
  fmap snd $ withFlintCtx b $ \bptr ctx ->
  f aptr bptr ctx

--------------------------------------------------
--- () -> FMPZ
--------------------------------------------------

{-# INLINE lift0FlintCtx #-}
lift0FlintCtx
  :: ( FlintWithContext ctx c )
  => ( Ptr (CFlint c) -> Ptr (CFlintCtx ctx) -> IO r )
  -> (c, r)
lift0FlintCtx f = unsafePerformIO $ withNewFlintCtx f

{-# INLINE lift0FlintCtx_ #-}
lift0FlintCtx_
  :: ( FlintWithContext ctx c )
  => ( Ptr (CFlint c) -> Ptr (CFlintCtx ctx) -> IO r )
  -> c
lift0FlintCtx_ = fst . lift0FlintCtx

--------------------------------------------------
--- FMPZ -> FMPZ
--------------------------------------------------

{-# INLINE liftFlintCtx #-}
liftFlintCtx
  :: ( FlintWithContext ctx c, FlintWithContext ctx a )
  => (    Ptr (CFlint c) -> Ptr (CFlint a)
       -> Ptr (CFlintCtx ctx)
       -> IO r )
  -> a
  -> (c, r)
liftFlintCtx f (!a) = unsafePerformIO $
  fmap snd $ withFlint a     $ \aptr     ->
             withNewFlintCtx $ \cptr ctx ->
  f cptr aptr ctx

{-# INLINE liftFlintCtx_ #-}
liftFlintCtx_
  :: ( FlintWithContext ctx c, FlintWithContext ctx a )
  => (    Ptr (CFlint c) -> Ptr (CFlint a)
       -> Ptr (CFlintCtx ctx)
       -> IO r )
  -> a
  -> c
liftFlintCtx_ = fst .: liftFlintCtx

--------------------------------------------------
--- FMPZ -> FMPZ -> FMPZ
--------------------------------------------------

{-# INLINE lift2FlintCtx #-}
lift2FlintCtx
  :: ( FlintWithContext ctx c
     , FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlint c)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> Ptr (CFlintCtx ctx)
       -> IO r )
  -> a -> b
  -> (c, r)
lift2FlintCtx f (!a) (!b) = unsafePerformIO $
    fmap snd $ withFlint a     $ \aptr     ->
    fmap snd $ withFlint b     $ \bptr     ->
               withNewFlintCtx $ \cptr ctx ->
    f cptr aptr bptr ctx

{-# INLINE lift2FlintCtx_ #-}
lift2FlintCtx_
  :: ( FlintWithContext ctx c
     , FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlint c)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> Ptr (CFlintCtx ctx)
       -> IO r )
  -> a -> b
  -> c
lift2FlintCtx_ = fst .:. lift2FlintCtx

{-# INLINE lift2FlintCtx' #-}
lift2FlintCtx'
  :: forall ctx a b r .
     ( FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlint a)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> Ptr (CFlintCtx ctx)
       -> IO r)
  -> a -> b
  -> r
lift2FlintCtx' f a b = snd cr
  where
  cr = lift2FlintCtx f a b
  _ = fst cr :: a

--------------------------------------------------
--- FMPZ -> FMPZ -> (FMPZ, FMPZ)
--------------------------------------------------

{-# INLINE lift2Flint2Ctx #-}
lift2Flint2Ctx
  :: ( FlintWithContext ctx c, FlintWithContext ctx d
     , FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlint c) -> Ptr (CFlint d)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> Ptr (CFlintCtx ctx)
       -> IO r )
  -> a -> b
  -> ((c,d), r)
lift2Flint2Ctx f (!a) (!b) = unsafePerformIO $
  fmap snd $ withFlint a     $ \aptr     ->
  fmap snd $ withFlint b     $ \bptr     ->
  fmap cmb $ withNewFlint    $ \cptr     -> 
             withNewFlintCtx $ \dptr ctx -> 
  f cptr dptr aptr bptr ctx
  where
   cmb (c, (d,r)) = ((c,d), r)

{-# INLINE lift2Flint2Ctx_ #-}
lift2Flint2Ctx_
  :: ( FlintWithContext ctx c, FlintWithContext ctx d
     , FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlint c) -> Ptr (CFlint d)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> Ptr (CFlintCtx ctx)
       -> IO r )
  -> a -> b
  -> (c,d)
lift2Flint2Ctx_ = fst .:. lift2Flint2Ctx

{-# INLINE lift2Flint2Ctx' #-}
lift2Flint2Ctx'
  :: forall ctx a b r .
     ( FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlint a) -> Ptr (CFlint a)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> Ptr (CFlintCtx ctx)
       -> IO r )
  -> a -> b
  -> r
lift2Flint2Ctx' f a b = snd cdr
  where
  cdr = lift2Flint2Ctx f a b
  _ = fst cdr :: (a,a)

--------------------------------------------------
--- FMPZ -> FMPZ -> (FMPZ, FMPZ, FMPZ)
--------------------------------------------------

{-# INLINE lift2Flint3Ctx #-}
lift2Flint3Ctx
  :: ( FlintWithContext ctx c, FlintWithContext ctx d
     , FlintWithContext ctx e
     , FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlint c) -> Ptr (CFlint d) -> Ptr (CFlint e)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> Ptr (CFlintCtx ctx)
       -> IO r )
  -> a -> b
  -> ((c,d,e), r)
lift2Flint3Ctx f (!a) (!b) = unsafePerformIO $
    fmap snd $ withFlint a     $ \aptr     ->
    fmap snd $ withFlint b     $ \bptr     ->
    fmap cmb $ withNewFlint    $ \cptr     ->
               withNewFlint    $ \dptr     ->
               withNewFlintCtx $ \eptr ctx ->
    f cptr dptr eptr aptr bptr ctx
  where
    cmb (c,(d,(e,r))) = ((c,d,e),r)

{-# INLINE lift2Flint3Ctx_ #-}
lift2Flint3Ctx_
  :: ( FlintWithContext ctx c, FlintWithContext ctx d
     , FlintWithContext ctx e
     , FlintWithContext ctx a, FlintWithContext ctx b)
  => (    Ptr (CFlint c) -> Ptr (CFlint d) -> Ptr (CFlint e)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> Ptr (CFlintCtx ctx)
       -> IO r )
  -> a -> b
  -> (c,d,e)
lift2Flint3Ctx_ = fst .:. lift2Flint3Ctx

{-# INLINE lift2Flint3Ctx' #-}
lift2Flint3Ctx'
  :: forall ctx a b r .
     ( FlintWithContext ctx a, FlintWithContext ctx b)
  => (    Ptr (CFlint a) -> Ptr (CFlint a) -> Ptr (CFlint a)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> Ptr (CFlintCtx ctx)
       -> IO r)
  -> a -> b
  -> r
lift2Flint3Ctx' f a b = snd cder where
  cder = lift2Flint3Ctx f a b
  _ = fst cder :: (a,a,a)
