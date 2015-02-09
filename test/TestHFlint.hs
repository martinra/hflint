module Main
where

import Test.Tasty ( defaultMain
                  , testGroup
                  )

import FMPZTests ( fmpzTestGroup )


main = defaultMain $
       testGroup "HFlint Tests" [fmpzTestGroup]
