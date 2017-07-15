module Lib
    ( someFunc
    , makeEmptyEnsemble
    ) where

import qualified Data.Array.Repa as R
import qualified Data.Map.Strict as Map


someFunc :: IO ()
someFunc = putStrLn "someFunc"


newtype Positions = Positions (R.Array R.U R.DIM2 Double)
    deriving Show

newtype Velocities = Velocities (R.Array R.U R.DIM2 Double)
    deriving Show

newtype ParameterName = ParameterName String
    deriving Show
data ParameterValue = EnsembleProperty (R.Array R.U R.DIM1 Double)
                    | ParticleProperty Double
    deriving Show
newtype Parameters = Parameters (Map.Map ParameterName ParameterValue)
    deriving Show

data Ensemble = Ensemble Positions Velocities Parameters
              | EmptyEnsemble
    deriving Show

makeEmptyEnsemble = Empty


