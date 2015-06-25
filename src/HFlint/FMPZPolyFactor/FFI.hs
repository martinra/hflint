{-# LINE 1 "FFI.pre.hsc" #-}
{-# LANGUAGE
{-# LINE 2 "FFI.pre.hsc" #-}
    CApiFFI
  , EmptyDataDecls
  , FlexibleInstances
  , ForeignFunctionInterface
  , MultiParamTypeClasses
  , TupleSections
  , TypeFamilies
  #-}

module HFlint.FMPZPolyFactor.FFI
where


{-# LINE 15 "FFI.pre.hsc" #-}

import Control.Monad.IO.Class ( liftIO )
import Foreign.C.Types ( CLong(..) )
import Foreign.ForeignPtr ( ForeignPtr
                          , mallocForeignPtr, withForeignPtr
                          , addForeignPtrFinalizer
                          )
import Foreign.Ptr ( Ptr, FunPtr, nullPtr )
import Foreign.Storable ( Storable(..) )

import HFlint.FMPZ.FFI
import HFlint.FMPZPoly.FFI
import HFlint.Internal.Context
import HFlint.Internal.Flint
import HFlint.Internal.FlintWithContext



{-# LINE 33 "FFI.pre.hsc" #-}


newtype FMPZPolyFactor = FMPZPolyFactor (ForeignPtr CFMPZPolyFactor)
type CFMPZPolyFactor = CFlint FMPZPolyFactor

instance FlintWithContext FlintTrivialContext FMPZPolyFactor where
  data CFlint FMPZPolyFactor

  newFlintCtx = liftIO $ do
    a <- mallocForeignPtr
    withForeignPtr a fmpz_poly_factor_init
    addForeignPtrFinalizer p_fmpz_poly_factor_clear a
    return $ FMPZPolyFactor a

  withFlintCtx (FMPZPolyFactor a) f = liftIO $
    withForeignPtr a $ \aptr ->
    f aptr nullPtr >>= return . (FMPZPolyFactor a,)


instance Flint FMPZPolyFactor

withFMPZPolyFactor
  :: FMPZPolyFactor -> (Ptr CFMPZPolyFactor -> IO b)
  -> IO (FMPZPolyFactor, b)
withFMPZPolyFactor = withFlint

withFMPZPolyFactor_
  :: FMPZPolyFactor -> (Ptr CFMPZPolyFactor -> IO b)
  -> IO FMPZPolyFactor
withFMPZPolyFactor_ = withFlint_

withNewFMPZPolyFactor
  :: (Ptr CFMPZPolyFactor -> IO b)
  -> IO (FMPZPolyFactor, b)
withNewFMPZPolyFactor = withNewFlint

withNewFMPZPolyFactor_
  :: (Ptr CFMPZPolyFactor -> IO b)
  -> IO FMPZPolyFactor
withNewFMPZPolyFactor_ = withNewFlint_


instance Storable CFMPZPolyFactor where
    sizeOf _ = (40)
{-# LINE 77 "FFI.pre.hsc" #-}
    alignment _ = 8
{-# LINE 78 "FFI.pre.hsc" #-}
    peek = error "CFMPZPolyFactor.peek: Not defined"
    poke = error "CFMPZPolyFactor.poke: Not defined"


foreign import ccall unsafe "fmpz_poly_factor_init"
        fmpz_poly_factor_init :: Ptr CFMPZPolyFactor -> IO ()

foreign import ccall unsafe "fmpz_poly_factor_init2"
        fmpz_poly_factor_init2 :: Ptr CFMPZPolyFactor -> CLong -> IO ()

foreign import ccall unsafe "fmpz_poly_factor_clear"
        fmpz_poly_factor_clear :: Ptr CFMPZPolyFactor -> IO ()

foreign import capi "flint/fmpz_poly.h value fmpz_poly_factor_clear"
        p_fmpz_poly_factor_clear :: FunPtr (Ptr CFMPZPolyFactor -> IO ())

foreign import ccall unsafe "fmpz_poly_factor_zassenhaus"
        fmpz_poly_factor_zassenhaus :: Ptr CFMPZPolyFactor -> Ptr CFMPZPoly -> IO ()


foreign import ccall unsafe "fmpz_poly_factor_get_content_additional"
        fmpz_poly_factor_get_content :: Ptr CFMPZ -> Ptr CFMPZPolyFactor -> IO ()

foreign import ccall unsafe "fmpz_poly_factor_number_factors_additional"
        fmpz_poly_factor_number_factors :: Ptr CFMPZPolyFactor -> IO CLong

foreign import ccall unsafe "fmpz_poly_factor_get_factor_additional"
        fmpz_poly_factor_get_factor :: Ptr CFMPZPoly -> Ptr CFMPZPolyFactor -> CLong -> IO CLong
