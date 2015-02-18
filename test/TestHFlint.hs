module Main
where

import Test.Tasty ( defaultMain
                  , testGroup
                  )

import FMPZTests ( fmpzTestGroup )
import FMPQTests ( fmpqTestGroup )
import FMPQPolyTests ( fmpqPolyTestGroup )


main = defaultMain $ testGroup "HFlint Tests"
       [ fmpzTestGroup
       , fmpqTestGroup
       , fmpqPolyTestGroup
       ]
