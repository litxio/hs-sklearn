{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies
    ,UndecidableInstances#-}

module SKLearn.Classes where

import Data.Coerce
import Data.Proxy
import GHC.TypeLits
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Repr.ForeignPtr as Repa
import qualified Data.Massiv.Array as Massiv
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import SKLearn.PyInterOp
import SKLearn.PyInterOp.Numpy

class (ToPyArgument arr, KnownNat rank)
  => ArrayLike rank arr | arr -> rank where
  convertToNumpy :: arr -> Py PyObject
  convertFromNumpy :: PyObject -> Py arr
instance ArrayLike 1 (Repa.Array Repa.F Repa.DIM1 Double) where
  convertToNumpy = repaToNumpy
  convertFromNumpy npArrObj = do
    len <- liftIO $ npArrayDim 0 npArrObj
    numpyToRepa npArrObj (Repa.ix1 $ fromIntegral len)
instance ArrayLike 2 (Repa.Array Repa.F Repa.DIM2 Double) where
  convertToNumpy = repaToNumpy
  convertFromNumpy npArrObj = do
    rows <- liftIO $ npArrayDim 0 npArrObj
    cols <- liftIO $ npArrayDim 1 npArrObj
    numpyToRepa npArrObj (Repa.ix2 (fromIntegral rows) (fromIntegral cols))
instance (KnownNat n, Massiv.Manifest r ix Double, ix ~ Massiv.Ix n
         ,Massiv.Mutable r ix Double)
  => ArrayLike n (Massiv.Array r ix Double) where
  convertToNumpy = massivToNumpy
  convertFromNumpy npArrObj = do
    let nDims = fromIntegral $ natVal (Proxy :: Proxy n)
    nDims' <- liftIO $ npArrayNDim npArrObj
    when (nDims /= nDims') $
      throwM $ LogicError $ "Numpy array has "<>show nDims'
                            <>" dimensions; expected "<>show nDims
    dims <- liftIO $
      foldM (\sz d -> npArrayDim d npArrObj 
               >>= Massiv.setSzM sz (fromIntegral $ nDims-d) . fromIntegral)
            Massiv.zeroSz
            [0..nDims-1]
    Massiv.compute <$> numpyToMassiv npArrObj dims


class BaseEstimator a where
  data Params a :: *
  new :: PyInterpreter -> Params a -> IO a
  -- getParams :: a -> IO (Params a)
  -- setParams :: a -> Params a -> IO ()


class BaseEstimator a => Regressor a where
  score :: (ArrayLike 2 matrix, ArrayLike 1 vector)
        => PyInterpreter -> a -> matrix -> vector -> IO Double
  default score :: (Coercible a PyObject, ArrayLike 2 matrix, ArrayLike 1 vector)
                => PyInterpreter -> a -> matrix -> vector -> IO Double
  score interpreter estm x y = runPython interpreter $ do
    simpleCallMethod (coerce estm) "score" [SomePyArgument x, SomePyArgument y]
      >>= fromPyDouble

  predict :: (ArrayLike 2 matrix, ArrayLike 1 vector)
          => PyInterpreter -> a -> matrix -> IO vector
  default predict :: (Coercible a PyObject, ArrayLike 2 matrix, ArrayLike 1 vector)
                  => PyInterpreter -> a -> matrix -> IO vector
  predict interpreter regressor mat = runPython interpreter $ do
    resArr <- simpleCallMethod (coerce regressor) "predict" [SomePyArgument mat]
    convertFromNumpy resArr -- (ix1 (head (listOfShape (extent mat))))


-- | This class doesn't actually exist in the Python sklearn hierarchy, but
-- many classes follow the pattern
class BaseEstimator a => Supervised a where
  fitS :: (ArrayLike 2 matrix, ArrayLike 1 vector)
       => PyInterpreter -> a -> matrix -> vector -> IO a
  default fitS :: (ArrayLike 2 matrix, ArrayLike 1 vector, Coercible a PyObject)
               => PyInterpreter -> a -> matrix -> vector -> IO a
  fitS interpreter estm x y = runPython interpreter $ do
    simpleCallMethod (coerce estm) "fit" [SomePyArgument x, SomePyArgument y]
    return estm


class BaseEstimator a => Unsupervised a where
  fitU :: ArrayLike 2 matrix => PyInterpreter -> a -> matrix -> IO a
  default fitU :: (ArrayLike 2 matrix, Coercible a PyObject)
               => PyInterpreter -> a -> matrix -> IO a
  fitU interpreter estm x = runPython interpreter $ do
    simpleCallMethod (coerce estm) "fit" [SomePyArgument x]
    return estm


