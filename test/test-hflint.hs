import Test.Tasty ( defaultMain
                  , testGroup
                  )

import FMPZTests ( FMPZTestGroup )


main = defaultMain $
       testGroup "HFlint Tests" [FMPZTestGroup]
