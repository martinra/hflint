{-# LANGUAGE
    EmptyDataDecls
  , TypeFamilies
  , FlexibleContexts
  , FlexibleInstances
  , ScopedTypeVariables
  #-}

module HFlint.Internal.Flint
  ( Flint(..)

 )
where

import Foreign.Ptr ( Ptr )


class Flint a where
  data CFlint a :: *

  newFlint :: IO a

  withFlint :: a
            -> (Ptr (CFlint a) -> IO b)
            -> IO (a, b)

  {-# INLINE withFlint_ #-}
  withFlint_ :: a
             -> (Ptr (CFlint a) -> IO b)
             -> IO a
  withFlint_ a f = fst <$> withFlint a f

  {-# INLINE withNewFlint #-}
  withNewFlint :: (Ptr (CFlint a) -> IO b)
               -> IO (a, b)
  withNewFlint f = flip withFlint f =<< newFlint

  {-# INLINE withNewFlint_ #-}
  withNewFlint_ :: (Ptr (CFlint a) -> IO b)
                -> IO a
  withNewFlint_ f = fst <$> withNewFlint f
