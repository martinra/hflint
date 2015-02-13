module Main
where

import Test.Tasty ( defaultMain
                  , testGroup
                  )

import FMPZTests ( fmpzTestGroup )
import FMPQTests ( fmpqTestGroup )

main = defaultMain $
       testGroup "HFlint Tests" [fmpzTestGroup, fmpqTestGroup]
