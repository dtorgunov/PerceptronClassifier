module Types where

type Weights = [Double]
type Input = [Double]
type Classification = Double
type ActivationFunction = Double -> Classification
-- type Network = Input -> Classification
data NetworkDesc = Hyperplane { plusPoint :: Input
                              , minusPoint :: Input
                              , c :: Double
                              }
                 | Union NetworkDesc NetworkDesc
                 | Intersection NetworkDesc NetworkDesc
                   deriving (Show, Eq)

type NetworkFunction = Input -> Double
                            
data Network = Network { f :: NetworkFunction
                       , net :: NetworkDesc
                       }

type TrainingInput = (Input, Classification)
type TrainingSet = [TrainingInput]
type PairedInputs = [[(TrainingInput, TrainingInput)]]

-- A function that will determine the order in which the points will be compaired
-- against one another
type PairFunction = [TrainingInput] -> [TrainingInput] -> PairedInputs


