module HFlint.Utility.Prelude
  ( module Import
  )
where

import Prelude as Import hiding
  ( (+), (-), negate, subtract
  , (*), (/), recip, (^), (^^)
  , gcd, lcm
  , quotRem, quot, rem
  )

import Control.Arrow       as Import
  ( (&&&), (***), first, second )
import Control.Applicative as Import
import Control.DeepSeq     as Import
import Control.Monad       as Import
import Control.Monad.Reader as Import
  ( Reader, ReaderT
  , ask
  , runReader, runReaderT
  )
import Control.Monad.Zip   as Import
import Data.Composition    as Import
  ( (.:), (.:.) )
import Data.Either         as Import
import Data.Foldable       as Import
import Data.Functor        as Import
import Data.Maybe          as Import
import Data.Monoid         as Import
import Data.MonoTraversable  as Import
import Data.Proxy          as Import
import Data.Reflection     as Import
import Data.Traversable    as Import
import Data.Vector as Import
  ( Vector )
import Foreign.Ptr         as Import
  ( Ptr )
import GHC.Exts            as Import
  ( IsList )
import Math.Structure      as Import
import System.IO.Unsafe    as Import
  ( unsafePerformIO )
