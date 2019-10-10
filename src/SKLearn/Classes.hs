
module SKLearn.Classes where

import Data.Coerce
import GHC.TypeLits
import Data.Array.Repa
import SKLearn.PyInterOp
import Data.Array.Repa.Repr.ForeignPtr


type Matrix a = Array F DIM2 a
type Vector a = Array F DIM1 a


class BaseEstimator a where
  data Params a :: *
  new :: Params a -> IO a
  -- getParams :: a -> IO (Params a)
  -- setParams :: a -> Params a -> IO ()


class BaseEstimator a => Regressor a where
  score :: a -> Matrix Double -> Vector Double -> IO Double
  predict :: a -> Matrix Double -> IO (Vector Double)
  default predict :: Coercible a PyObject 
                  => a -> Matrix Double -> IO (Vector Double)
  predict regressor mat = do -- withGIL $ do
    resArr <- simpleCallMethod (coerce regressor) "predict" [SomePyArgument mat]
    numpyToRepa resArr (ix1 (head (listOfShape (extent mat))))


-- | This class doesn't actually exist in the Python sklearn hierarchy, but
-- many classes follow the pattern
class BaseEstimator a => Supervised a where
  fitS :: a -> Matrix Double -> Vector Double -> IO a
  default fitS :: Coercible a PyObject 
               => a -> Matrix Double -> Vector Double -> IO a
  fitS estm x y = do -- withGIL $ do
    simpleCallMethod (coerce estm) "fit" [SomePyArgument x, SomePyArgument y]
    return estm


class BaseEstimator a => Unsupervised a where
  fitU :: a -> Matrix Double -> IO a
  default fitU :: Coercible a PyObject => a -> Matrix Double -> IO a
  fitU estm x = do -- withGIL $ do
    simpleCallMethod (coerce estm) "fit" [SomePyArgument x]
    return estm


