module HFlint.FMPZ.Limbs
where

import Prelude hiding ( fromInteger )

import qualified Prelude as P

import Control.Applicative ( (<$>) )
import Control.Monad ( forM_
                     , when
                     )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Loops ( untilM )
import Control.Monad.Writer ( tell
                            , WriterT
                            , execWriterT )
import Data.List ( foldl' )
import Data.Monoid ( Dual(..) )
import Foreign.Ptr ( Ptr )
import Foreign.C.Types ( CULong(..) )
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPZ.Basic ()
import HFlint.FMPZ.FFI
import HFlint.FMPZ.Internal ( withFMPZ
                            , withNewFMPZ
                            , withNewFMPZ_ )

data Sign = Positive
          | Zero
          | Negative

data LimbRepr = LimbRepr Sign [CULong]

limbSize :: Integral a => a
limbSize = P.fromInteger . fromIntegral $ 1 + div (maxBound :: CULong) 2


fromInteger :: Integer -> LimbRepr
fromInteger a = case compare a 0 of
  GT -> LimbRepr Positive $ fromInteger' a
  EQ -> LimbRepr Zero []
  LT -> LimbRepr Negative $ fromInteger' (-a)
  where
  fromInteger' b = map fromIntegral $
                   snd $ head $ dropWhile ((/=0) . fst) $
                   iterate nextLimb (b,[])
  nextLimb (b,ls) = let (q,r) = b `divMod` limbSize
                    in (q,r:ls)

fromFMPZ :: FMPZ -> LimbRepr
fromFMPZ a = unsafePerformIO $ fmap snd $
             withFMPZ a $ const fromCFMPZ

fromCFMPZ :: Ptr CFMPZ -> IO LimbRepr
fromCFMPZ aptr = fmap snd $ withNewFMPZ $ const $ \cptr -> do
  fmpz_set cptr aptr
  sgn <- fmpz_sgn aptr
  when (sgn < 0) $ fmpz_neg cptr cptr
  Dual ls <- execWriterT $ untilM
             (nextLimb cptr)
             (liftIO $ (==1) <$> fmpz_is_zero cptr)
  return $ case compare sgn 0 of
    GT -> LimbRepr Positive ls
    EQ -> LimbRepr Zero []
    LT -> LimbRepr Negative ls
  where
  nextLimb :: Ptr CFMPZ -> WriterT (Dual [CULong]) IO ()
  nextLimb bptr = do
    -- todo: as soon fmpz_fdiv_qr_ui is implemented us it
    tell =<< liftIO (Dual <$> (:[]) <$> fmpz_fdiv_ui bptr limbSize)
    liftIO $ fmpz_fdiv_q_ui bptr bptr limbSize

toInteger :: LimbRepr -> Integer
toInteger (LimbRepr _ [])   = 0
toInteger (LimbRepr sgn ls) = let n = foldl' (\a l -> a*limbSize + l) 0 $
                                             map fromIntegral ls
                              in case sgn of
                                Positive -> n 
                                Zero     -> 0
                                Negative -> -n 

toNewFMPZ :: LimbRepr -> FMPZ
toNewFMPZ ls = unsafePerformIO . withNewFMPZ_ $ const $ \cptr ->
               toCFMPZ cptr ls

toCFMPZ :: Ptr CFMPZ -> LimbRepr -> IO ()
toCFMPZ cptr (LimbRepr _ []) = fmpz_zero cptr
toCFMPZ cptr (LimbRepr sgn (l:ls)) = do
  fmpz_set_ui cptr l
  forM_ ls $ \l' -> do fmpz_mul_ui cptr cptr limbSize
                       fmpz_add_ui cptr cptr l'
  case sgn of
    Positive -> return ()
    Zero     -> fmpz_zero cptr
    Negative -> fmpz_neg cptr cptr

