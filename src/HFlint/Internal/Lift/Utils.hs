module HFlint.Internal.Lift.Utils
where

import Control.Monad.Reader
import Data.Functor.Identity
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.Internal.Context


--------------------------------------------------
-- unsafePerformIO within ReaderT
--------------------------------------------------

{-# INLINE fromIO #-}
fromIO :: RIOFlint ctx a -> RFlint ctx a
fromIO = mapReaderT (Identity . unsafePerformIO)

--------------------------------------------------
-- functions which are constant in the last argument
--------------------------------------------------

constBack :: (a -> c) -> (a -> b -> c)
constBack ff a _ = ff a

constBack2 :: (a -> b -> d) -> (a -> b -> c -> d)
constBack2 ff a b _ = ff a b

constBack3 :: (a -> b -> c -> e) -> (a -> b -> c -> d -> e)
constBack3 ff a b c _ = ff a b c

constBack4 :: (a -> b -> c -> d -> f) -> (a -> b -> c -> d -> e -> f)
constBack4 ff a b c d _ = ff a b c d

constBack5 :: (a -> b -> c -> d -> e -> g) -> (a -> b -> c -> d -> e -> f -> g)
constBack5 ff a b c d e _ = ff a b c d e
