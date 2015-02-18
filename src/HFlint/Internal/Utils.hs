module HFlint.Internal.Utils
where

import Control.Exception ( Exception
                         , throw
                         )

throwBeforeIf :: Exception e
               => e -> (a -> Bool)
               -> (a -> b) -> a -> b
throwBeforeIf e cond f a =
  if cond a then throw e else f a

throwBeforeIf2 :: Exception e
               => e -> (a -> b -> Bool)
               -> (a -> b -> c) -> a -> b -> c
throwBeforeIf2 e cond f a b =
  if cond a b then throw e else f a b
