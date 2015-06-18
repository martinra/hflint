module HFlint.NMod.Internal
where


instance Flint NMod where
  type CFlint NMod = CNMod
  type FlintType NMod = NModType
  type CFlintType NMod = CNModType

  flintType = NModType . ask

  newFlint _ = return (return 0)

  -- fixme: the result also lies in the reader monad
  withFlint (NMod a) f = do
    ta <- ask
    withForeignPtr a $
      f ta >=> \r -> return (NMod a,r)

withFMPZ :: FMPZ -> (Ptr CFMPZType -> Ptr CFMPZ -> IO b)
         -> IO (FMPZ, b)
withFMPZ = withFlint 

withFMPZ_ :: FMPZ -> (Ptr CFMPZType -> Ptr CFMPZ -> IO b)
          -> IO FMPZ
withFMPZ_ = withFlint_

withNewFMPZ :: (Ptr CFMPZType -> Ptr CFMPZ -> IO b)
            -> IO (FMPZ, b)
withNewFMPZ = withNewFlint FMPZType

withNewFMPZ_ :: (Ptr CFMPZType -> Ptr CFMPZ -> IO b)
             -> IO FMPZ
withNewFMPZ_ = withNewFlint_ FMPZType
