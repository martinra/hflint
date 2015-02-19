module Main
where

import Test.Tasty ( defaultMain
                  , testGroup
                  )

import FMPQTests ( fmpqTestGroup )
import FMPZTests ( fmpzTestGroup )
import FMPQPolyTests ( fmpqPolyTestGroup )
import FMPZPolyTests ( fmpzPolyTestGroup )


main = defaultMain $ testGroup "HFlint Tests"
       [ fmpzTestGroup
       , fmpqTestGroup
       , fmpzPolyTestGroup
       , fmpqPolyTestGroup
       ]
