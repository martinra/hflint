module HFlint.Internal.Utils
where

import Control.Exception ( Exception
                         , throw
                         )

{-# INLINE throwBeforeIf #-}
throwBeforeIf :: Exception e
               => e -> (a -> Bool)
               -> (a -> b) -> a -> b
throwBeforeIf e cond f a =
  if cond a then throw e else f a

{-# INLINE throwBeforeIf2 #-}
throwBeforeIf2 :: Exception e
               => e -> (a -> b -> Bool)
               -> (a -> b -> c) -> a -> b -> c
throwBeforeIf2 e cond f a b =
  if cond a b then throw e else f a b


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
