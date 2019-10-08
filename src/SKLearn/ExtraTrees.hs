{-# LANGUAGE DeriveAnyClass #-}

module SKLearn.ExtraTrees where

import GHC.Generics
import Data.Aeson
import SKLearn.Classes

data ExtraTreesRegressor = ExtraTreesRegressor deriving (Show, Generic, ToJSON)

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

data ExtraTreesRegressorParams =
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

instance BaseEstimator ExtraTreesRegressor where
  type Params ExtraTreesRegressor = ExtraTreesRegressorParams
  new params = pyNew "sklearn.ensemble" "ExtraTreesRegressor" (toJSON params)

pyNew = undefined

instance Regressor ExtraTreesRegressor where


