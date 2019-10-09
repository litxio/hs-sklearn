{-# LANGUAGE DeriveAnyClass #-}

module SKLearn.ExtraTrees where

import GHC.Generics
import qualified Data.HashMap.Strict as HM
import Data.Array.Repa
import Data.Aeson
import SKLearn.Classes
import SKLearn.PyInterOp

data ExtraTreesRegressor = ExtraTreesRegressor {_pyObj :: PyObjectPtr}
  deriving (Show, Generic)

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
  type Params ExtraTreesRegressor = ExtraTreesRegressorParams
  new params = ExtraTreesRegressor <$>
                pyNew "sklearn.ensemble" "ExtraTreesRegressor" (toJSON params)


instance Regressor ExtraTreesRegressor where
  predict (ExtraTreesRegressor etPtr) mat = do
    state <- pyGILStateEnsure
    npArr <- repaToNumpy mat
    resArr <- callMethod etPtr $
      PyCallRequest "predict" [SomePyArgument npArr] HM.empty
    pyGILStateRelease state
    numpyToRepa resArr (ix1 (head (listOfShape (extent mat))))

instance Supervised ExtraTreesRegressor where
  fitS (ExtraTreesRegressor ptr) x y = do
    state <- pyGILStateEnsure
    xP <- repaToNumpy x
    yP <- repaToNumpy y
    let args = [SomePyArgument xP, SomePyArgument yP]
    callMethod ptr $ PyCallRequest "fit" args HM.empty
    pyGILStateRelease state
    return $ ExtraTreesRegressor ptr

    


