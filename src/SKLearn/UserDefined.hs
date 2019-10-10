{-# LANGUAGE DeriveAnyClass #-}

module SKLearn.UserDefined where

import GHC.Generics
import qualified Data.HashMap.Strict as HM
import Data.Array.Repa
import Data.Aeson
import SKLearn.Classes
import SKLearn.PyInterOp


newtype UserDefinedRegressor = UserDefinedRegressor { _pyObj :: PyObject}
  deriving (Generic)


instance BaseEstimator UserDefinedRegressor where
  data Params UserDefinedRegressor = UDRegressorParams 
                                        { pyModule :: String
                                        , pyCallable :: String
                                        , arguments :: Value}
  new UDRegressorParams{pyModule, pyCallable, arguments} =
    UserDefinedRegressor <$> pyNew pyModule pyCallable arguments


instance Regressor UserDefinedRegressor

instance Supervised UserDefinedRegressor

