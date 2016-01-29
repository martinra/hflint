{-# LANGUAGE
    BangPatterns
  , TypeFamilies
  , FlexibleContexts
  , FlexibleInstances
  , ScopedTypeVariables
  #-}

module HFlint.Internal.LiftCtx
  ( liftFlintCtx0
  , lift2FlintCtx0

  , lift0FlintCtx
  , lift0FlintCtx_

  , liftFlintCtx
  , liftFlintCtx_

  , lift2FlintCtx
  , lift2FlintCtx_
  , lift2FlintCtx'

  , lift2FlintCtx2
  , lift2FlintCtx2_
  , lift2FlintCtx2'

  , lift2FlintCtx3
  , lift2FlintCtx3_
  , lift2FlintCtx3'
  )
where

import Data.Composition
import Foreign.Ptr ( Ptr )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.Internal.Context
import HFlint.Internal.FlintWithContext


--------------------------------------------------
-- FMPZ -> ()
--------------------------------------------------

{-# INLINE liftFlintCtx0 #-}
liftFlintCtx0
  :: ( FlintWithContext ctx a
     , ReifiesFlintContext ctx ctxProxy )
  => ( Ptr (CFlintCtx a) -> Ptr (CFlintContext ctx) -> IO r )
  -> a ctxProxy
  -> r
liftFlintCtx0 f (!a) = unsafePerformIO $ snd <$> withFlintCtx a f

{-# INLINE lift2FlintCtx0 #-}
lift2FlintCtx0
  :: ( FlintWithContext ctx a, FlintWithContext ctx b
     , ReifiesFlintContext ctx ctxProxy )
  => (    Ptr (CFlintCtx a) -> Ptr (CFlintCtx b)
       -> Ptr (CFlintContext ctx)
       -> IO r )
  -> a ctxProxy -> b ctxProxy
  -> r
lift2FlintCtx0 f (!a) (!b) = unsafePerformIO $
  fmap snd $ withFlintCtx a $ \aptr _   ->
  fmap snd $ withFlintCtx b $ \bptr ctx ->
  f aptr bptr ctx

--------------------------------------------------
--- () -> FMPZ
--------------------------------------------------

{-# INLINE lift0FlintCtx #-}
lift0FlintCtx
  :: ( FlintWithContext ctx c
     , ReifiesFlintContext ctx ctxProxy )
  => ( Ptr (CFlintCtx c) -> Ptr (CFlintContext ctx) -> IO r )
  -> (c ctxProxy, r)
lift0FlintCtx f = unsafePerformIO $ withNewFlintCtx f

{-# INLINE lift0FlintCtx_ #-}
lift0FlintCtx_
  :: ( FlintWithContext ctx c
     , ReifiesFlintContext ctx ctxProxy )
  => ( Ptr (CFlintCtx c) -> Ptr (CFlintContext ctx) -> IO r )
  -> c ctxProxy
lift0FlintCtx_ = fst . lift0FlintCtx

--------------------------------------------------
--- FMPZ -> FMPZ
--------------------------------------------------

{-# INLINE liftFlintCtx #-}
liftFlintCtx
  :: ( FlintWithContext ctx c, FlintWithContext ctx a
     , ReifiesFlintContext ctx ctxProxy )
  => (    Ptr (CFlintCtx c) -> Ptr (CFlintCtx a)
       -> Ptr (CFlintContext ctx)
       -> IO r )
  -> a ctxProxy
  -> (c ctxProxy, r)
liftFlintCtx f (!a) = unsafePerformIO $
  fmap snd $ withFlintCtx a  $ \aptr _    ->
             withNewFlintCtx $ \cptr ctx ->
  f cptr aptr ctx

{-# INLINE liftFlintCtx_ #-}
liftFlintCtx_
  :: ( FlintWithContext ctx c, FlintWithContext ctx a
     , ReifiesFlintContext ctx ctxProxy )
  => (    Ptr (CFlintCtx c) -> Ptr (CFlintCtx a)
       -> Ptr (CFlintContext ctx)
       -> IO r )
  -> a ctxProxy
  -> c ctxProxy
liftFlintCtx_ = fst .: liftFlintCtx

--------------------------------------------------
--- FMPZ -> FMPZ -> FMPZ
--------------------------------------------------

{-# INLINE lift2FlintCtx #-}
lift2FlintCtx
  :: ( FlintWithContext ctx c
     , FlintWithContext ctx a, FlintWithContext ctx b
     , ReifiesFlintContext ctx ctxProxy )
  => (    Ptr (CFlintCtx c)
       -> Ptr (CFlintCtx a) -> Ptr (CFlintCtx b)
       -> Ptr (CFlintContext ctx)
       -> IO r )
  -> a ctxProxy -> b ctxProxy
  -> (c ctxProxy, r)
lift2FlintCtx f (!a) (!b) = unsafePerformIO $
    fmap snd $ withFlintCtx a  $ \aptr _   ->
    fmap snd $ withFlintCtx b  $ \bptr _   ->
               withNewFlintCtx $ \cptr ctx ->
    f cptr aptr bptr ctx

{-# INLINE lift2FlintCtx_ #-}
lift2FlintCtx_
  :: ( FlintWithContext ctx c
     , FlintWithContext ctx a, FlintWithContext ctx b
     , ReifiesFlintContext ctx ctxProxy )
  => (    Ptr (CFlintCtx c)
       -> Ptr (CFlintCtx a) -> Ptr (CFlintCtx b)
       -> Ptr (CFlintContext ctx)
       -> IO r )
  -> a ctxProxy -> b ctxProxy
  -> c ctxProxy
lift2FlintCtx_ = fst .:. lift2FlintCtx

{-# INLINE lift2FlintCtx' #-}
lift2FlintCtx'
  :: forall ctx ctxProxy a b r .
     ( FlintWithContext ctx a, FlintWithContext ctx b
     , ReifiesFlintContext ctx ctxProxy )
  => (    Ptr (CFlintCtx a)
       -> Ptr (CFlintCtx a) -> Ptr (CFlintCtx b)
       -> Ptr (CFlintContext ctx)
       -> IO r)
  -> a ctxProxy -> b ctxProxy
  -> r
lift2FlintCtx' f a b = snd cr
  where
  cr = lift2FlintCtx f a b
  _ = fst cr :: a ctxProxy

--------------------------------------------------
--- FMPZ -> FMPZ -> (FMPZ, FMPZ)
--------------------------------------------------

{-# INLINE lift2FlintCtx2 #-}
lift2FlintCtx2
  :: ( FlintWithContext ctx c, FlintWithContext ctx d
     , FlintWithContext ctx a, FlintWithContext ctx b
     , ReifiesFlintContext ctx ctxProxy )
  => (    Ptr (CFlintCtx c) -> Ptr (CFlintCtx d)
       -> Ptr (CFlintCtx a) -> Ptr (CFlintCtx b)
       -> Ptr (CFlintContext ctx)
       -> IO r )
  -> a ctxProxy -> b ctxProxy
  -> ((c ctxProxy,d ctxProxy), r)
lift2FlintCtx2 f (!a) (!b) = unsafePerformIO $
  fmap snd $ withFlintCtx a  $ \aptr _   ->
  fmap snd $ withFlintCtx b  $ \bptr _   ->
  fmap cmb $ withNewFlintCtx $ \cptr _   -> 
             withNewFlintCtx $ \dptr ctx -> 
  f cptr dptr aptr bptr ctx
  where
   cmb (c, (d,r)) = ((c,d), r)

{-# INLINE lift2FlintCtx2_ #-}
lift2FlintCtx2_
  :: ( FlintWithContext ctx c, FlintWithContext ctx d
     , FlintWithContext ctx a, FlintWithContext ctx b
     , ReifiesFlintContext ctx ctxProxy )
  => (    Ptr (CFlintCtx c) -> Ptr (CFlintCtx d)
       -> Ptr (CFlintCtx a) -> Ptr (CFlintCtx b)
       -> Ptr (CFlintContext ctx)
       -> IO r )
  -> a ctxProxy -> b ctxProxy
  -> (c ctxProxy,d ctxProxy)
lift2FlintCtx2_ = fst .:. lift2FlintCtx2

{-# INLINE lift2FlintCtx2' #-}
lift2FlintCtx2'
  :: forall ctx ctxProxy a b r .
     ( FlintWithContext ctx a, FlintWithContext ctx b
     , ReifiesFlintContext ctx ctxProxy )
  => (    Ptr (CFlintCtx a) -> Ptr (CFlintCtx a)
       -> Ptr (CFlintCtx a) -> Ptr (CFlintCtx b)
       -> Ptr (CFlintContext ctx)
       -> IO r )
  -> a ctxProxy -> b ctxProxy
  -> r
lift2FlintCtx2' f a b = snd cdr
  where
  cdr = lift2FlintCtx2 f a b
  _ = fst cdr :: (a ctxProxy,a ctxProxy)

--------------------------------------------------
--- FMPZ -> FMPZ -> (FMPZ, FMPZ, FMPZ)
--------------------------------------------------

{-# INLINE lift2FlintCtx3 #-}
lift2FlintCtx3
  :: ( FlintWithContext ctx c, FlintWithContext ctx d
     , FlintWithContext ctx e
     , FlintWithContext ctx a, FlintWithContext ctx b
     , ReifiesFlintContext ctx ctxProxy )
  => (    Ptr (CFlintCtx c) -> Ptr (CFlintCtx d) -> Ptr (CFlintCtx e)
       -> Ptr (CFlintCtx a) -> Ptr (CFlintCtx b)
       -> Ptr (CFlintContext ctx)
       -> IO r )
  -> a ctxProxy -> b ctxProxy
  -> ((c ctxProxy,d ctxProxy,e ctxProxy), r)
lift2FlintCtx3 f (!a) (!b) = unsafePerformIO $
    fmap snd $ withFlintCtx a  $ \aptr _   ->
    fmap snd $ withFlintCtx b  $ \bptr _   ->
    fmap cmb $ withNewFlintCtx $ \cptr _   ->
               withNewFlintCtx $ \dptr _   ->
               withNewFlintCtx $ \eptr ctx ->
    f cptr dptr eptr aptr bptr ctx
  where
    cmb (c,(d,(e,r))) = ((c,d,e),r)

{-# INLINE lift2FlintCtx3_ #-}
lift2FlintCtx3_
  :: ( FlintWithContext ctx c, FlintWithContext ctx d
     , FlintWithContext ctx e
     , FlintWithContext ctx a, FlintWithContext ctx b
     , ReifiesFlintContext ctx ctxProxy )
  => (    Ptr (CFlintCtx c) -> Ptr (CFlintCtx d) -> Ptr (CFlintCtx e)
       -> Ptr (CFlintCtx a) -> Ptr (CFlintCtx b)
       -> Ptr (CFlintContext ctx)
       -> IO r )
  -> a ctxProxy -> b ctxProxy
  -> (c ctxProxy,d ctxProxy,e ctxProxy)
lift2FlintCtx3_ = fst .:. lift2FlintCtx3

{-# INLINE lift2FlintCtx3' #-}
lift2FlintCtx3'
  :: forall ctx ctxProxy a b r .
     ( FlintWithContext ctx a, FlintWithContext ctx b
     , ReifiesFlintContext ctx ctxProxy )
  => (    Ptr (CFlintCtx a) -> Ptr (CFlintCtx a) -> Ptr (CFlintCtx a)
       -> Ptr (CFlintCtx a) -> Ptr (CFlintCtx b)
       -> Ptr (CFlintContext ctx)
       -> IO r)
  -> a ctxProxy -> b ctxProxy
  -> r
lift2FlintCtx3' f a b = snd cder where
  cder = lift2FlintCtx3 f a b
  _ = fst cder :: (a ctxProxy,a ctxProxy,a ctxProxy)
