
module SKLearn.Classes where

import GHC.TypeLits
import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr


type Matrix a = Array F DIM2 a
type Vector a = Array F DIM1 a


class BaseEstimator a where
  type Params a :: *
  new :: Params a -> IO a
  -- getParams :: a -> IO (Params a)
  -- setParams :: a -> Params a -> IO ()


class BaseEstimator a => Regressor a where
  score :: a -> Matrix Double -> Vector Double -> IO Double
  predict :: a -> Matrix Double -> IO (Vector Double)


-- | This class doesn't actually exist in the Python sklearn hierarchy, but
-- many classes follow the pattern
class BaseEstimator a => Supervised a where
  fitS :: a -> Matrix Double -> Vector Double -> IO a


class BaseEstimator a => Unsupervised a where
  fitU :: a -> Matrix Double -> IO a


