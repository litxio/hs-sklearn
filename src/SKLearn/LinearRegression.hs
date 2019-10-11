{-# LANGUAGE DeriveAnyClass #-}

module SKLearn.LinearRegression where

import GHC.Generics
import qualified Data.HashMap.Strict as HM
import Data.Array.Repa
import Data.Aeson
import SKLearn.Classes
import SKLearn.PyInterOp

newtype LinearRegression = LinearRegression {_pyObj :: PyObject}
  deriving (Generic)

defaultLinearRegressionParams = LinearRP Nothing Nothing Nothing

instance BaseEstimator LinearRegression where
  data Params LinearRegression =
          LinearRP { fitIntercept :: Maybe Bool
                   , normalize :: Maybe Bool
                   , nJobs :: Maybe Int
                   } deriving (Eq, Show, Generic, ToJSON)
  new interpreter params = LinearRegression <$>
                runPython interpreter (pyNew "sklearn.linear_model"
                                             "LinearRegression"
                                             (toJSON params))

instance Regressor LinearRegression

instance Supervised LinearRegression

-- hello I'm a bug
