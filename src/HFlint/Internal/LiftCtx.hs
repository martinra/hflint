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

import Foreign.Ptr ( Ptr )
import Data.Composition

import HFlint.Internal.Context
import HFlint.Internal.FlintWithContext
import HFlint.Internal.Lift.Utils


--------------------------------------------------
-- FMPZ -> ()
--------------------------------------------------

{-# INLINE liftFlint0Ctx #-}
liftFlint0Ctx
  :: ( FlintWithContext ctx a )
  => ( Ptr (CFlint a) -> Ptr (CFlintCtx ctx) -> IO r )
  -> RFlint ctx a
  -> RFlint ctx r
liftFlint0Ctx f (!a) = do
  a' <- a
  fromIO $
    fmap snd $ withFlintCtx a' f

{-# INLINE lift2Flint0Ctx #-}
lift2Flint0Ctx
  :: ( FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlint a) -> Ptr (CFlint b)
       -> Ptr (CFlintCtx ctx)
       -> IO r )
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx r
lift2Flint0Ctx f (!a) (!b) = do
  a' <- a
  b' <- b
  fromIO $
    fmap snd $ withFlintImplicitCtx a' $ \aptr     ->
    fmap snd $ withFlintCtx b'         $ \bptr ctx ->
    f aptr bptr ctx

--------------------------------------------------
--- () -> FMPZ
--------------------------------------------------

{-# INLINE lift0FlintCtx #-}
lift0FlintCtx
  :: ( FlintWithContext ctx c )
  => ( Ptr (CFlint c) -> Ptr (CFlintCtx ctx) -> IO r )
  -> RFlint ctx (c, r)
lift0FlintCtx f = 
  fromIO $
    withNewFlintCtx $ \cptr ctx ->
    f cptr ctx

{-# INLINE lift0FlintCtx_ #-}
lift0FlintCtx_
  :: ( FlintWithContext ctx c )
  => ( Ptr (CFlint c) -> Ptr (CFlintCtx ctx) -> IO r )
  -> RFlint ctx c
lift0FlintCtx_ = fmap fst . lift0FlintCtx

--------------------------------------------------
--- FMPZ -> FMPZ
--------------------------------------------------

{-# INLINE liftFlintCtx #-}
liftFlintCtx
  :: ( FlintWithContext ctx c, FlintWithContext ctx a )
  => (    Ptr (CFlint c) -> Ptr (CFlint a)
       -> Ptr (CFlintCtx ctx)
       -> IO r )
  -> RFlint ctx a
  -> RFlint ctx (c, r)
liftFlintCtx f (!a) = do
  a' <- a
  fromIO $
    withNewFlintImplicitCtx    $ \cptr     ->
    fmap snd $ withFlintCtx a' $ \aptr ctx ->
    f cptr aptr ctx

{-# INLINE liftFlintCtx_ #-}
liftFlintCtx_
  :: ( FlintWithContext ctx c, FlintWithContext ctx a )
  => (    Ptr (CFlint c) -> Ptr (CFlint a)
       -> Ptr (CFlintCtx ctx)
       -> IO r )
  -> RFlint ctx a
  -> RFlint ctx c
liftFlintCtx_ = fmap fst .: liftFlintCtx

--------------------------------------------------
--- FMPZ -> FMPZ -> FMPZ
--------------------------------------------------

{-# INLINE lift2FlintCtx #-}
lift2FlintCtx
  :: ( FlintWithContext ctx c
     , FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlint c) -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> Ptr (CFlintCtx ctx)
       -> IO r )
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx (c, r)
lift2FlintCtx f (!a) (!b) = do
  a' <- a; b' <- b
  fromIO $
    fmap snd $ withFlintImplicitCtx a' $ \aptr     ->
    fmap snd $ withFlintImplicitCtx b' $ \bptr     ->
               withNewFlintCtx         $ \cptr ctx ->
    f cptr aptr bptr ctx

{-# INLINE lift2FlintCtx_ #-}
lift2FlintCtx_
  :: ( FlintWithContext ctx c
     , FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlint c) -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> Ptr (CFlintCtx ctx)
       -> IO r )
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx c
lift2FlintCtx_ = fmap fst .:. lift2FlintCtx

{-# INLINE lift2FlintCtx' #-}
lift2FlintCtx'
  :: forall ctx a b r .
     ( FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlint a) -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> Ptr (CFlintCtx ctx)
       -> IO r)
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx r
lift2FlintCtx' f a b = fmap snd cr
  where
  cr = lift2FlintCtx f a b
  _ = fmap fst cr :: RFlint ctx a

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
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx ((c,d), r)
lift2Flint2Ctx f (!a) (!b) = do
  a' <- a; b' <- b
  fromIO $
    fmap snd $ withFlintImplicitCtx a' $ \aptr     ->
    fmap snd $ withFlintImplicitCtx b' $ \bptr     ->
    fmap cmb $ withNewFlintImplicitCtx $ \cptr     -> 
               withNewFlintCtx         $ \dptr ctx -> 
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
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx (c,d)
lift2Flint2Ctx_ = fmap fst .:. lift2Flint2Ctx

{-# INLINE lift2Flint2Ctx' #-}
lift2Flint2Ctx'
  :: forall ctx a b r .
     ( FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlint a) -> Ptr (CFlint a)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> Ptr (CFlintCtx ctx)
       -> IO r )
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx r
lift2Flint2Ctx' f a b = fmap snd cdr
  where
  cdr = lift2Flint2Ctx f a b
  _ = fmap fst cdr :: RFlint ctx (a,a)

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
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx ((c,d,e), r)
lift2Flint3Ctx f (!a) (!b) = do
  a' <- a; b' <- b
  fromIO $
    fmap snd $ withFlintImplicitCtx a' $ \aptr     ->
    fmap snd $ withFlintImplicitCtx b' $ \bptr     ->
    fmap cmb $ withNewFlintImplicitCtx $ \cptr     ->
               withNewFlintImplicitCtx $ \dptr     ->
               withNewFlintCtx         $ \eptr ctx ->
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
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx (c,d,e)
lift2Flint3Ctx_ = fmap fst .:. lift2Flint3Ctx

{-# INLINE lift2Flint3Ctx' #-}
lift2Flint3Ctx'
  :: forall ctx a b r .
     ( FlintWithContext ctx a, FlintWithContext ctx b)
  => (    Ptr (CFlint a) -> Ptr (CFlint a) -> Ptr (CFlint a)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> Ptr (CFlintCtx ctx)
       -> IO r)
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx r
lift2Flint3Ctx' f a b = fmap snd cder where
  cder = lift2Flint3Ctx f a b
  _ = fmap fst cder :: RFlint ctx (a,a,a)
