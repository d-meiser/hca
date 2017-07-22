module Hca
    ( emptyEnsemble
    , combineEnsembles
    ) where

import qualified Data.Array.Repa as R
import qualified Data.Map.Strict as Map


type PhaseSpaceShape = R.DIM2
type PhaseSpaceArray = R.Array R.U PhaseSpaceShape Double

newtype PhaseSpace = PhaseSpace {array :: PhaseSpaceArray}
    deriving Show

newtype ParameterName = ParameterName String
    deriving Show
data ParameterValue = EnsembleProperty (R.Array R.U R.DIM2 Double)
                    | ParticleProperty Double
    deriving Show
newtype Parameters = Parameters (Map.Map ParameterName ParameterValue)
    deriving Show

data Ensemble = Ensemble PhaseSpace
              | EmptyEnsemble
    deriving Show


emptyEnsemble :: Ensemble
emptyEnsemble = EmptyEnsemble

combineEnsembles :: Ensemble -> Ensemble -> Ensemble
combineEnsembles EmptyEnsemble b = b
combineEnsembles a EmptyEnsemble = a
combineEnsembles (Ensemble ph0) (Ensemble ph1) = Ensemble (PhaseSpace ph)
  where
    ph :: PhaseSpaceArray
    ph = R.computeUnboxedS $
          R.reshape (R.Z R.:. (6::Int) R.:. newSize) mergedRows
    newSize :: Int
    newSize = ((R.size $ R.extent $ array ph0) + (R.size $ R.extent $ array ph1)) `quot` 6
    mergedRows = foldr R.append emptyArray rows
    rows = [R.slice phaseSpaceArray (R.Any R.:. i R.:. R.All)
            | phaseSpaceArray <- [array ph0, array ph1]
            , i <- [0..(5::Int)]]
    emptyArray = R.delay $ R.fromListUnboxed (R.Z R.:. (0::Int)) []

