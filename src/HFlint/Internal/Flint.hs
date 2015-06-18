{-# LANGUAGE
    EmptyDataDecls
  , TypeFamilies
  , FlexibleContexts
  , FlexibleInstances
  , ScopedTypeVariables
  #-}

module HFlint.Internal.Flint
  ( Flint(..)

  , FlintTrivialContext
  , CFlintTrivialContext
  , runTrivialContext
  )
where

import Control.Monad.Reader
import Foreign.Ptr ( Ptr, nullPtr )

import HFlint.Internal.FlintWithContext


data FlintTrivialContext = FlintTrivialContext
instance FlintContext FlintTrivialContext where
  data CFlintCtx FlintTrivialContext

  implicitCtx f ctxptr aptr = runReaderT (f aptr) ctxptr

type CFlintTrivialContext = CFlintCtx FlintTrivialContext

runTrivialContext :: Monad m => ReaderT (Ptr CFlintTrivialContext) m a -> m a
runTrivialContext a = runReaderT a nullPtr 


class FlintWithContext FlintTrivialContext a => Flint a where
    newFlint :: IO a
    newFlint = runTrivialContext newFlintCtx 

    withFlint :: a
              -> (Ptr (CFlint a) -> IO b)
              -> IO (a, b)
    withFlint a f = runTrivialContext $ withFlintCtx a (const f)

    withFlint_ :: a
               -> (Ptr (CFlint a) -> IO b)
               -> IO a
    withFlint_ a f = runTrivialContext $ withFlintCtx_ a (const f)

    withNewFlint :: (Ptr (CFlint a) -> IO b)
                 -> IO (a, b)
    withNewFlint f = runTrivialContext $ withNewFlintCtx (const f)

    withNewFlint_ :: (Ptr (CFlint a) -> IO b)
                  -> IO a
    withNewFlint_ f = runTrivialContext $ withNewFlintCtx_ (const f)

--------------------------------------------------
-- FMPZ -> ()
--------------------------------------------------

-- liftFlint0 :: Flint a
--            => ( Ptr (CFlintType a) -> Ptr (CFlint a) -> IO r)
--            -> a -> r 
-- liftFlint0 f (!a) = unsafePerformIO $ snd <$> withFlint a f
-- 
-- lift2Flint0 :: ( Flint a, Flint b )
--             => (  Ptr (CFlintType a)
--                -> Ptr (CFlint a) -> Ptr (CFlint b)
--                -> IO r)
--             -> a -> b -> r
-- lift2Flint0 f (!a) (!b) = unsafePerformIO $ fmap snd $
--                           withFlint a $ \atype aptr -> fmap snd $
--                           withFlint b $ \_ bptr ->
--                           f atype aptr bptr
-- 
-- --------------------------------------------------
-- --- () -> FMPZ
-- --------------------------------------------------
-- 
-- lift0FlintWithType :: ( Flint c )
--                    => FlintType c
--                    -> (  Ptr (CFlintType c)
--                       -> Ptr (CFlint c)
--                       -> IO r)
--                    -> (c, r)
-- lift0FlintWithType t f =
--   unsafePerformIO $
--   withNewFlint t $ \ctype cptr ->
--   f ctype cptr
-- 
-- lift0FlintWithType_ :: ( Flint c )
--                     => FlintType c
--                     -> (  Ptr (CFlintType c)
--                        -> Ptr (CFlint c)
--                        -> IO r)
--                     -> c
-- lift0FlintWithType_ = fst .: lift0FlintWithType
-- 
-- --------------------------------------------------
-- --- FMPZ -> FMPZ
-- --------------------------------------------------
-- 
-- liftFlintWithType :: ( Flint c, Flint a )
--                   => FlintType c
--                   -> (  Ptr (CFlintType c)
--                      -> Ptr (CFlint c) -> Ptr (CFlint a)
--                      -> IO r)
--                   -> a -> (c, r)
-- liftFlintWithType t f (!a) =
--   unsafePerformIO $
--   withNewFlint t $ \ctype cptr -> fmap snd $
--   withFlint a $ \_ aptr ->
--   f ctype cptr aptr
-- 
-- liftFlintWithType_ :: ( Flint c, Flint a )
--                    => FlintType c
--                    -> (  Ptr (CFlintType c)
--                       -> Ptr (CFlint c) -> Ptr (CFlint a)
--                       -> IO r)
--                    -> a -> c
-- liftFlintWithType_ = fst .:. liftFlintWithType
-- 
-- 
-- liftFlint :: ( Flint c, Flint a
--              , FlintType c ~ FlintType a )
--           => (  Ptr (CFlintType c)
--              -> Ptr (CFlint c) -> Ptr (CFlint a)
--              -> IO r)
--           -> a -> (c, r)
-- liftFlint f a = liftFlintWithType (flintType a) f a
-- 
-- liftFlint_ :: ( Flint c, Flint a
--               , FlintType c ~ FlintType a )
--            => (  Ptr (CFlintType c)
--               -> Ptr (CFlint c)
--               -> Ptr (CFlint a)
--               -> IO r)
--            -> a -> c
-- liftFlint_ f a = liftFlintWithType_ (flintType a) f a
-- 
-- --------------------------------------------------
-- --- FMPZ -> FMPZ -> FMPZ
-- --------------------------------------------------
-- 
-- lift2FlintWithType :: ( Flint c, Flint a, Flint b)
--                    => FlintType c
--                    -> (  Ptr (CFlintType c)
--                       -> Ptr (CFlint c)
--                       -> Ptr (CFlint a) -> Ptr (CFlint b)
--                       -> IO r)
--                    -> a -> b -> (c, r)
-- lift2FlintWithType t f (!a) (!b) =
--   unsafePerformIO $
--   withNewFlint t $ \ctype cptr -> fmap snd $
--   withFlint a $ \_ aptr -> fmap snd $
--   withFlint b $ \_ bptr ->
--   f ctype cptr aptr bptr
-- 
-- lift2FlintWithType_ :: ( Flint c, Flint a, Flint b)
--                     => FlintType c
--                     -> (  Ptr (CFlintType c)
--                        -> Ptr (CFlint c)
--                        -> Ptr (CFlint a) -> Ptr (CFlint b)
--                        -> IO r)
--                     -> a -> b -> c
-- lift2FlintWithType_ = fst .:: lift2FlintWithType
-- 
-- 
-- lift2Flint :: ( Flint c, Flint a, Flint b
--               , FlintType c ~ FlintType a )
--            => (  Ptr (CFlintType c)
--               -> Ptr (CFlint c)
--               -> Ptr (CFlint a) -> Ptr (CFlint b)
--               -> IO r)
--            -> a -> b -> (c, r)
-- lift2Flint f a b = lift2FlintWithType (flintType a) f a b
-- 
-- lift2Flint_ :: ( Flint c, Flint a, Flint b
--                , FlintType c ~ FlintType a, FlintType c ~ FlintType b )
--             => (  Ptr (CFlintType c)
--                -> Ptr (CFlint c)
--                -> Ptr (CFlint a) -> Ptr (CFlint b)
--                -> IO r)
--             -> a -> b -> c
-- lift2Flint_ f a b = lift2FlintWithType_ (flintType a) f a b
-- 
-- lift2Flint' :: forall a b r .
--                ( Flint a, Flint b )
--             => (  Ptr (CFlintType a)
--                -> Ptr (CFlint a)
--                -> Ptr (CFlint a) -> Ptr (CFlint b)
--                -> IO r)
--             -> a -> b -> r
-- lift2Flint' f a b = snd cr
--   where
--   cr = lift2Flint f a b
--   _ = fst cr :: a
-- 
-- --------------------------------------------------
-- --- FMPZ -> FMPZ -> (FMPZ, FMPZ)
-- --------------------------------------------------
-- 
-- lift2Flint2WithType :: ( Flint c, Flint d, Flint a, Flint b )
--                     => FlintType c -> FlintType d
--                     -> (  Ptr (CFlintType c) -> Ptr (CFlintType d)
--                        -> Ptr (CFlint c) -> Ptr (CFlint d)
--                        -> Ptr (CFlint a) -> Ptr (CFlint b)
--                        -> IO r )
--                     -> a -> b -> ((c,d), r)
-- lift2Flint2WithType tc td f (!a) (!b) = ((c,d), r)
--   where
--   (c, (d,r)) = 
--     unsafePerformIO $
--     withNewFlint tc $ \ctype cptr ->
--     withNewFlint td $ \dtype dptr -> fmap snd $
--     withFlint a $ \_ aptr -> fmap snd $
--     withFlint b $ \_ bptr ->
--     f ctype dtype cptr dptr aptr bptr
-- 
-- lift2Flint2WithType_ :: ( Flint c, Flint d, Flint a, Flint b )
--                      => FlintType c -> FlintType d
--                      -> (  Ptr (CFlintType c) -> Ptr (CFlintType d)
--                         -> Ptr (CFlint c) -> Ptr (CFlint d)
--                         -> Ptr (CFlint a) -> Ptr (CFlint b)
--                         -> IO r )
--                      -> a -> b -> (c,d)
-- lift2Flint2WithType_ = fst .::. lift2Flint2WithType
-- 
-- 
-- lift2Flint2 :: ( Flint c, Flint d, Flint a, Flint b
--                , FlintType c ~ FlintType a, FlintType d ~ FlintType a )
--             => (  Ptr (CFlintType c) -> Ptr (CFlintType d)
--                -> Ptr (CFlint c) -> Ptr (CFlint d)
--                -> Ptr (CFlint a) -> Ptr (CFlint b)
--                -> IO r)
--             -> a -> b -> ((c,d), r)
-- lift2Flint2 f a b = lift2Flint2WithType (flintType a) (flintType a) f a b
-- 
-- lift2Flint2_ :: ( Flint c, Flint d, Flint a, Flint b
--                 , FlintType c ~ FlintType a, FlintType d ~ FlintType a )
--              => (  Ptr (CFlintType c) -> Ptr (CFlintType d)
--                 -> Ptr (CFlint c) -> Ptr (CFlint d)
--                 -> Ptr (CFlint a) -> Ptr (CFlint b)
--                 -> IO r)
--              -> a -> b -> (c,d)
-- lift2Flint2_ = fst .:. lift2Flint2 
-- 
-- lift2Flint2' :: forall a b r .
--                 ( Flint a, Flint b )
--              => (  Ptr (CFlintType a) -> Ptr (CFlintType a)
--                 -> Ptr (CFlint a) -> Ptr (CFlint a)
--                 -> Ptr (CFlint a) -> Ptr (CFlint b)
--                 -> IO r)
--              -> a -> b -> r
-- lift2Flint2' f a b = snd cdr
--   where
--   cdr = lift2Flint2 f a b
--   _ = fst cdr :: (a,a)
-- 
-- --------------------------------------------------
-- --- FMPZ -> FMPZ -> (FMPZ, FMPZ, FMPZ)
-- --------------------------------------------------
-- 
-- lift2Flint3WithType :: ( Flint c, Flint d, Flint e, Flint a, Flint b )
--                     => FlintType c -> FlintType d -> FlintType e
--                     -> (  Ptr (CFlintType c) -> Ptr (CFlintType d) -> Ptr (CFlintType e)
-- 
--                        -> Ptr (CFlint c) -> Ptr (CFlint d) -> Ptr (CFlint e)
--                        -> Ptr (CFlint a) -> Ptr (CFlint b)
--                        -> IO r )
--                     -> a -> b -> ((c,d,e), r)
-- lift2Flint3WithType tc td te f (!a) (!b) = ((c,d,e), r)
--   where
--   (c, (d, (e,r))) = 
--     unsafePerformIO $
--     withNewFlint tc $ \ctype cptr ->
--     withNewFlint td $ \dtype dptr ->
--     withNewFlint te $ \etype eptr -> fmap snd $
--     withFlint a $ \_ aptr -> fmap snd $
--     withFlint b $ \_ bptr ->
--     f ctype dtype etype cptr dptr eptr aptr bptr
-- 
-- lift2Flint3WithType_ :: ( Flint c, Flint d, Flint e, Flint a, Flint b )
--                      => FlintType c -> FlintType d -> FlintType e
--                      -> (  Ptr (CFlintType c) -> Ptr (CFlintType d) -> Ptr (CFlintType e)
-- 
--                         -> Ptr (CFlint c) -> Ptr (CFlint d) -> Ptr (CFlint e)
--                         -> Ptr (CFlint a) -> Ptr (CFlint b)
--                         -> IO r )
--                      -> a -> b -> (c,d,e)
-- lift2Flint3WithType_ = fst .::: lift2Flint3WithType
-- 
-- lift2Flint3 :: ( Flint c, Flint d, Flint e, Flint a, Flint b
--                , FlintType c ~ FlintType a, FlintType d ~ FlintType a
--                , FlintType e ~ FlintType a )
--             => (  Ptr (CFlintType c) -> Ptr (CFlintType d) -> Ptr (CFlintType e)
--                -> Ptr (CFlint c) -> Ptr (CFlint d) -> Ptr (CFlint e)
--                -> Ptr (CFlint a) -> Ptr (CFlint b)
--                -> IO r)
--             -> a -> b -> ((c,d,e), r)
-- lift2Flint3 f a b = lift2Flint3WithType (flintType a) (flintType a) (flintType a) f a b
-- 
-- 
-- lift2Flint3_ :: ( Flint c, Flint d, Flint e, Flint a, Flint b
--                , FlintType c ~ FlintType a, FlintType d ~ FlintType a
--                , FlintType e ~ FlintType a )
--             => (  Ptr (CFlintType c) -> Ptr (CFlintType d) -> Ptr (CFlintType e)
--                -> Ptr (CFlint c) -> Ptr (CFlint d) -> Ptr (CFlint e)
--                -> Ptr (CFlint a) -> Ptr (CFlint b)
--                -> IO r)
--             -> a -> b -> (c,d,e)
-- lift2Flint3_ = fst .:. lift2Flint3
-- 
-- lift2Flint3' :: forall a b r .
--                ( Flint a, Flint b )
--             => (  Ptr (CFlintType a) -> Ptr (CFlintType a) -> Ptr (CFlintType a)
--                -> Ptr (CFlint a) -> Ptr (CFlint a) -> Ptr (CFlint a)
--                -> Ptr (CFlint a) -> Ptr (CFlint b)
--                -> IO r)
--             -> a -> b -> r
-- lift2Flint3' f a b = snd cder
--   where
--   cder = lift2Flint3 f a b
--   _ = fst cder :: (a,a,a)
