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

import Control.Monad.Reader
import Foreign.Ptr ( Ptr )
import Data.Composition
import Data.Functor.Identity
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.Internal.FlintWithContext


--------------------------------------------------
-- unsafePerformIO within ReaderT
--------------------------------------------------

fromIO :: RIOFlint ctx a -> RFlint ctx a
fromIO = mapReaderT (Identity . unsafePerformIO)

--------------------------------------------------
-- FMPZ -> ()
--------------------------------------------------

liftFlint0Ctx
  :: ( FlintWithContext ctx a )
  => ( Ptr (CFlintCtx ctx) -> Ptr (CFlint a) -> IO r )
  -> RFlint ctx a
  -> RFlint ctx r
liftFlint0Ctx f (!a) = do
  a' <- a
  fromIO $
    fmap snd $ withFlintCtx a' f

lift2Flint0Ctx
  :: ( FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlintCtx ctx)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx r
lift2Flint0Ctx f (!a) (!b) = do
  a' <- a
  b' <- b
  fromIO $
    fmap snd $ withFlintImplicitCtx a' $ \    aptr ->
    fmap snd $ withFlintCtx b'         $ \ctx bptr ->
    f ctx aptr bptr

--------------------------------------------------
--- () -> FMPZ
--------------------------------------------------

lift0FlintCtx
  :: ( FlintWithContext ctx c )
  => (  Ptr (CFlintCtx ctx) -> Ptr (CFlint c) -> IO r )
  -> RFlint ctx (c, r)
lift0FlintCtx f = 
  fromIO $
    withNewFlintCtx $ \ctx cptr ->
    f ctx cptr

lift0FlintCtx_
  :: ( FlintWithContext ctx c )
  => (  Ptr (CFlintCtx ctx) -> Ptr (CFlint c) -> IO r )
  -> RFlint ctx c
lift0FlintCtx_ = fmap fst . lift0FlintCtx

--------------------------------------------------
--- FMPZ -> FMPZ
--------------------------------------------------

liftFlintCtx
  :: ( FlintWithContext ctx c, FlintWithContext ctx a )
  => (    Ptr (CFlintCtx ctx)
       -> Ptr (CFlint c) -> Ptr (CFlint a)
       -> IO r )
  -> RFlint ctx a
  -> RFlint ctx (c, r)
liftFlintCtx f (!a) = do
  a' <- a
  fromIO $
    withNewFlintImplicitCtx    $ \    cptr ->
    fmap snd $ withFlintCtx a' $ \ctx aptr ->
    f ctx cptr aptr

liftFlintCtx_
  :: ( FlintWithContext ctx c, FlintWithContext ctx a )
  => ( Ptr (CFlintCtx ctx)
       -> Ptr (CFlint c) -> Ptr (CFlint a)
       -> IO r )
  -> RFlint ctx a
  -> RFlint ctx c
liftFlintCtx_ = fmap fst .: liftFlintCtx

--------------------------------------------------
--- FMPZ -> FMPZ -> FMPZ
--------------------------------------------------

lift2FlintCtx
  :: ( FlintWithContext ctx c
     , FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlintCtx ctx)
       -> Ptr (CFlint c) -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx (c, r)
lift2FlintCtx f (!a) (!b) = do
  a' <- a; b' <- b
  fromIO $
    fmap snd $ withFlintImplicitCtx a' $ \    aptr ->
    fmap snd $ withFlintImplicitCtx b' $ \    bptr ->
               withNewFlintCtx         $ \ctx cptr ->
    f ctx cptr aptr bptr

lift2FlintCtx_
  :: ( FlintWithContext ctx c
     , FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlintCtx ctx)
       -> Ptr (CFlint c) -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx c
lift2FlintCtx_ = fmap fst .:. lift2FlintCtx


lift2FlintCtx'
  :: forall ctx a b r .
     ( FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlintCtx ctx)
       -> Ptr (CFlint a) -> Ptr (CFlint a) -> Ptr (CFlint b)
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

lift2Flint2Ctx
  :: ( FlintWithContext ctx c, FlintWithContext ctx d
     , FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlintCtx ctx)
       -> Ptr (CFlint c) -> Ptr (CFlint d)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx ((c,d), r)
lift2Flint2Ctx f (!a) (!b) = do
  a' <- a; b' <- b
  fromIO $
    fmap snd $ withFlintImplicitCtx a' $ \    aptr ->
    fmap snd $ withFlintImplicitCtx b' $ \    bptr ->
    fmap cmb $ withNewFlintImplicitCtx $ \    cptr -> 
               withNewFlintCtx         $ \ctx dptr -> 
    f ctx cptr dptr aptr bptr
  where
   cmb (c, (d,r)) = ((c,d), r)

lift2Flint2Ctx_
  :: ( FlintWithContext ctx c, FlintWithContext ctx d
     , FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlintCtx ctx)
       -> Ptr (CFlint c) -> Ptr (CFlint d)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx (c,d)
lift2Flint2Ctx_ = fmap fst .:. lift2Flint2Ctx

lift2Flint2Ctx'
  :: forall ctx a b r .
     ( FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlintCtx ctx)
       -> Ptr (CFlint a) -> Ptr (CFlint a)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
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

lift2Flint3Ctx
  :: ( FlintWithContext ctx c, FlintWithContext ctx d
     , FlintWithContext ctx e
     , FlintWithContext ctx a, FlintWithContext ctx b )
  => (    Ptr (CFlintCtx ctx)
       -> Ptr (CFlint c) -> Ptr (CFlint d) -> Ptr (CFlint e)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx ((c,d,e), r)
lift2Flint3Ctx f (!a) (!b) = do
  a' <- a; b' <- b
  fromIO $
    fmap snd $ withFlintImplicitCtx a' $ \    aptr ->
    fmap snd $ withFlintImplicitCtx b' $ \    bptr ->
    fmap cmb $ withNewFlintImplicitCtx $ \    cptr ->
               withNewFlintImplicitCtx $ \    dptr ->
               withNewFlintCtx         $ \ctx eptr ->
    f ctx cptr dptr eptr aptr bptr
  where
    cmb (c,(d,(e,r))) = ((c,d,e),r)

lift2Flint3Ctx_
  :: ( FlintWithContext ctx c, FlintWithContext ctx d
     , FlintWithContext ctx e
     , FlintWithContext ctx a, FlintWithContext ctx b)
  => (    Ptr (CFlintCtx ctx)
       -> Ptr (CFlint c) -> Ptr (CFlint d) -> Ptr (CFlint e)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx (c,d,e)
lift2Flint3Ctx_ = fmap fst .:. lift2Flint3Ctx

lift2Flint3Ctx'
  :: forall ctx a b r .
     ( FlintWithContext ctx a, FlintWithContext ctx b)
  => (    Ptr (CFlintCtx ctx)
       -> Ptr (CFlint a) -> Ptr (CFlint a) -> Ptr (CFlint a)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r)
  -> RFlint ctx a -> RFlint ctx b
  -> RFlint ctx r
lift2Flint3Ctx' f a b = fmap snd cder
  where
  cder = lift2Flint3Ctx f a b
  _ = fmap fst cder :: RFlint ctx (a,a,a)
