{-# LANGUAGE
    BangPatterns
  , TypeFamilies
  , FlexibleContexts
  , FlexibleInstances
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

import Foreign.Ptr ( Ptr )
import Data.Functor.Identity

import HFlint.Internal.Flint
import HFlint.Internal.FlintWithContext
import HFlint.Internal.LiftCtx


--------------------------------------------------
-- FMPZ -> ()
--------------------------------------------------

{-# INLINE liftFlint0 #-}
liftFlint0
  :: ( Flint a )
  => ( Ptr (CFlint a) -> IO r )
  -> a
  -> r
liftFlint0 f (!a) = runIdentity $ runTrivialContext $
  liftFlint0Ctx (const f) (return a)

{-# INLINE lift2Flint0 #-}
lift2Flint0
  :: ( Flint a, Flint b )
  => (    Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> a -> b
  -> r
lift2Flint0 f (!a) (!b) = runIdentity $ runTrivialContext $
  lift2Flint0Ctx (const f) (return a) (return b)

--------------------------------------------------
--- () -> FMPZ
--------------------------------------------------

{-# INLINE lift0Flint #-}
lift0Flint
  :: ( Flint c )
  => ( Ptr (CFlint c) -> IO r )
  -> (c, r)
lift0Flint f = runIdentity $ runTrivialContext $
  lift0FlintCtx (const f)

{-# INLINE lift0Flint_ #-}
lift0Flint_
  :: ( Flint c )
  => ( Ptr (CFlint c) -> IO r )
  -> c
lift0Flint_ f = runIdentity $ runTrivialContext $
  lift0FlintCtx_ (const f)

--------------------------------------------------
--- FMPZ -> FMPZ
--------------------------------------------------

{-# INLINE liftFlint #-}
liftFlint
  :: ( Flint c, Flint a )
  => ( Ptr (CFlint c) -> Ptr (CFlint a) -> IO r )
  -> a
  -> (c, r)
liftFlint f (!a) = runIdentity $ runTrivialContext $
  liftFlintCtx (const f) (return a)

{-# INLINE liftFlint_ #-}
liftFlint_
  :: ( Flint c, Flint a )
  => ( Ptr (CFlint c) -> Ptr (CFlint a) -> IO r )
  -> a
  -> c
liftFlint_ f (!a) = runIdentity $ runTrivialContext $
  liftFlintCtx_ (const f) (return a)

--------------------------------------------------
--- FMPZ -> FMPZ -> FMPZ
--------------------------------------------------

{-# INLINE lift2Flint #-}
lift2Flint
  :: ( Flint c, Flint a, Flint b )
  => (    Ptr (CFlint c) -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> a -> b -> (c, r)
lift2Flint f (!a) (!b) = runIdentity $ runTrivialContext $
  lift2FlintCtx (const f) (return a) (return b)

{-# INLINE lift2Flint_ #-}
lift2Flint_
  :: ( Flint c, Flint a, Flint b )
  => (    Ptr (CFlint c) -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> a -> b
  -> c
lift2Flint_ f (!a) (!b) = runIdentity $ runTrivialContext $
  lift2FlintCtx_ (const f) (return a) (return b)

{-# INLINE lift2Flint' #-}
lift2Flint'
  :: ( Flint a, Flint b )
  => (    Ptr (CFlint a) -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r)
  -> a -> b
  -> r
lift2Flint' f (!a) (!b) = runIdentity $ runTrivialContext $
  lift2FlintCtx' (const f) (return a) (return b)

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
lift2Flint2 f (!a) (!b) = runIdentity $ runTrivialContext $
  lift2Flint2Ctx (const f) (return a) (return b)

{-# INLINE lift2Flint2_ #-}
lift2Flint2_
  :: ( Flint c, Flint d, Flint a, Flint b )
  => (    Ptr (CFlint c) -> Ptr (CFlint d)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> a -> b
  -> (c,d)
lift2Flint2_ f (!a) (!b) = runIdentity $ runTrivialContext $
  lift2Flint2Ctx_ (const f) (return a) (return b)

{-# INLINE lift2Flint2' #-}
lift2Flint2'
  :: ( Flint a, Flint b )
  => (    Ptr (CFlint a) -> Ptr (CFlint a)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> a -> b
  -> r
lift2Flint2' f (!a) (!b) = runIdentity $ runTrivialContext $
  lift2Flint2Ctx' (const f) (return a) (return b)

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
lift2Flint3 f (!a) (!b) = runIdentity $ runTrivialContext $
  lift2Flint3Ctx (const f) (return a) (return b)

{-# INLINE lift2Flint3_ #-}
lift2Flint3_
  :: ( Flint c, Flint d, Flint e, Flint a, Flint b )
  => (    Ptr (CFlint c) -> Ptr (CFlint d) -> Ptr (CFlint e)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r )
  -> a -> b
  -> (c,d,e)
lift2Flint3_ f (!a) (!b) = runIdentity $ runTrivialContext $
  lift2Flint3Ctx_ (const f) (return a) (return b)

{-# INLINE lift2Flint3' #-}
lift2Flint3'
  :: ( Flint a, Flint b )
  => (    Ptr (CFlint a) -> Ptr (CFlint a) -> Ptr (CFlint a)
       -> Ptr (CFlint a) -> Ptr (CFlint b)
       -> IO r)
  -> a -> b
  -> r
lift2Flint3' f (!a) (!b) = runIdentity $ runTrivialContext $
  lift2Flint3Ctx' (const f) (return a) (return b)
