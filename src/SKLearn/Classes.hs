
module SKLearn.Classes where

import Data.Array.Repa


type Matrix r n m a = Array r (Z :. n :. m) a
type Vector r n a = Array r (Z :. n) a


class BaseEstimator a where
  type Params a :: *
  new :: Params a -> IO a
  -- getParams :: a -> IO (Params a)
  -- setParams :: a -> Params a -> IO ()


class BaseEstimator a => Regressor a where
  score :: a -> Matrix r n m Double -> Vector r m Double -> IO Double
  predict :: a -> Matrix r n m Double -> IO (Vector r m Double)


-- | This class doesn't actually exist in the Python sklearn hierarchy, but
-- many classes follow the pattern
class BaseEstimator a => Supervised a where
  fitS :: a -> Matrix r n m Double -> Vector r m Double -> IO a


class BaseEstimator a => Unsupervised a where
  fitU :: a -> Matrix r n m Double -> IO a


