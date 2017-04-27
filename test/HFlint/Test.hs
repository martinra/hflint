module Main
where

import Test.Tasty ( defaultMain
                  , testGroup
                  )

import HFlint.Test.FMPQ     as FMPQ
import HFlint.Test.FMPZ     as FMPZ
import HFlint.Test.FMPQPoly as FMPQPoly
import HFlint.Test.FMPZPoly as FMPZPoly
import HFlint.Test.Primes   as Primes


main = defaultMain $ testGroup "HFlint tests"
       [ FMPQ.tests
       , FMPZ.tests
       , FMPQPoly.tests
       , FMPZPoly.tests
       , Primes.tests
       ]
