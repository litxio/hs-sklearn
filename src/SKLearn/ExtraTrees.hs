{-# LANGUAGE DeriveAnyClass #-}

module SKLearn.ExtraTrees where

import GHC.Generics
import qualified Data.HashMap.Strict as HM
import Data.Array.Repa
import Data.Aeson
import SKLearn.Classes
import SKLearn.PyInterOp

newtype ExtraTreesRegressor = ExtraTreesRegressor {_pyObj :: PyObject}
  deriving (Generic)

data SplitCriterion = MSE | MAE deriving (Eq, Show, Enum, Generic, ToJSON)

data AbsoluteOrFraction = Absolute Int | Fraction Double
  deriving (Eq, Show, Generic, ToJSON)

data MaxFeatures = AbsoluteMax Int
                 | FractionalMax Double
                 | Auto
                 | Sqrt
                 | Log2
                 | AllFeatures
  deriving (Eq, Show, Generic, ToJSON)

defaultExtraTreesRegressorParams = ExtraTreesRP
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing

instance BaseEstimator ExtraTreesRegressor where
  data Params ExtraTreesRegressor =
          ExtraTreesRP { nEstimators :: Maybe Int
                       , criterion :: Maybe SplitCriterion
                       , maxDepth :: Maybe Int
                       , minSamplesSplit :: Maybe AbsoluteOrFraction
                       , minSamplesLeaf :: Maybe AbsoluteOrFraction
                       , minWeightFractionLeaf :: Maybe Double
                       , maxFeatures :: Maybe MaxFeatures
                       , maxLeafNodes :: Maybe Int
                       , minImpurityDecrease :: Maybe Double
                       , minImpuritySplit :: Maybe Float
                       , bootstrap :: Maybe Bool
                       , oobScore :: Maybe Bool
                       , nJobs :: Maybe Int
                       , randomState :: Maybe Int
                       , verbose :: Maybe Int
                       , warmStart :: Maybe Bool }
                       deriving (Eq, Show, Generic, ToJSON)
  new interpreter params = ExtraTreesRegressor <$>
                runPython interpreter (pyNew "sklearn.ensemble"
                                             "ExtraTreesRegressor"
                                             (toJSON params))

instance Regressor ExtraTreesRegressor

instance Supervised ExtraTreesRegressor

