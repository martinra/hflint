module Flint.FMPQMat.Internal
where


import Flint.Internal.Flint

import Flint.FMPQ.FFI

import Flint.FMPQMat.FFI

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CLong)

import Data.Foldable (forM_)


withFMPQMat :: FMPQMat -> (Ptr CFMPQMatType -> Ptr CFMPQMat -> IO b) -> IO (FMPQMat, b)
withFMPQMat = withFlint

withFMPQMat_ :: FMPQMat -> (Ptr CFMPQMatType -> Ptr CFMPQMat -> IO b) -> IO FMPQMat
withFMPQMat_ = withFlint_

withNewFMPQMat :: Int -> Int -> (Ptr CFMPQMatType -> Ptr CFMPQMat -> IO b) -> IO (FMPQMat, b)
withNewFMPQMat r c = withNewFlint (FMPQType, fromIntegral r, fromIntegral c)

withNewFMPQMat_ :: Int -> Int -> (Ptr CFMPQMatType -> Ptr CFMPQMat -> IO b) -> IO FMPQMat
withNewFMPQMat_ r c = withNewFlint_ (FMPQType, fromIntegral r, fromIntegral c)


-- todo: this should be implemented using windows as soon as they are
-- available
-- note: this is unsafe as the corresponding C function will likely be
setSubmatrix :: Ptr CFMPQMat -> CLong -> CLong ->
                Ptr CFMPQMat ->
                IO ()
setSubmatrix cptr ic jc aptr = do
  ra <- fmpq_mat_nrows aptr
  ca <- fmpq_mat_ncols aptr
  setSubmatrix' cptr ic jc aptr 0 0 ra ca

setSubmatrix' :: Ptr CFMPQMat -> CLong -> CLong ->
                Ptr CFMPQMat -> CLong -> CLong -> CLong -> CLong ->
                IO ()
setSubmatrix' cptr ic jc aptr ia ja ra ca =
    forM_ (zip [ic..ic+ra] [ia..ia+ra]) $ \(ixc,ixa) ->
    forM_ (zip [jc..jc+ca] [ja..ja+ca]) $ \(jxc,jxa) -> do
        ce <- fmpq_mat_entryref cptr (fromIntegral ixc) (fromIntegral $ jxc)
        ae <- fmpq_mat_entryref aptr (fromIntegral ixa) (fromIntegral jxa)
        fmpq_set ce ae
