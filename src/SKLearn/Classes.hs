
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
  new :: PyInterpreter -> Params a -> IO a
  -- getParams :: a -> IO (Params a)
  -- setParams :: a -> Params a -> IO ()


class BaseEstimator a => Regressor a where
  score :: PyInterpreter -> a -> Matrix Double -> Vector Double -> IO Double
  predict :: PyInterpreter -> a -> Matrix Double -> IO (Vector Double)
  default predict :: Coercible a PyObject 
                  => PyInterpreter -> a -> Matrix Double -> IO (Vector Double)
  predict interpreter regressor mat = runPython interpreter $ do
    resArr <- simpleCallMethod (coerce regressor) "predict" [SomePyArgument mat]
    numpyToRepa resArr (ix1 (head (listOfShape (extent mat))))


-- | This class doesn't actually exist in the Python sklearn hierarchy, but
-- many classes follow the pattern
class BaseEstimator a => Supervised a where
  fitS :: PyInterpreter -> a -> Matrix Double -> Vector Double -> IO a
  default fitS :: Coercible a PyObject 
               => PyInterpreter -> a -> Matrix Double -> Vector Double -> IO a
  fitS interpreter estm x y = runPython interpreter $ do
    simpleCallMethod (coerce estm) "fit" [SomePyArgument x, SomePyArgument y]
    return estm


class BaseEstimator a => Unsupervised a where
  fitU :: PyInterpreter -> a -> Matrix Double -> IO a
  default fitU :: PyInterpreter -> Coercible a PyObject => a -> Matrix Double -> IO a
  fitU interpreter estm x = runPython interpreter $ do
    simpleCallMethod (coerce estm) "fit" [SomePyArgument x]
    return estm


