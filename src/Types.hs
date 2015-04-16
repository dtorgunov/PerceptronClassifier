module Types where

type Weights = [Double]
type Inputs = [Double]
type Classification = Double
type ActivationFunction = Double -> Classification
type Network = Inputs -> Classification

type TrainingInput = (Inputs, Classification)
type TrainingSet = [TrainingInput]
type PairedInputs = [(TrainingInput, TrainingInput)]

-- A function that will determine the order in which the points will be compaired
-- against one another
type PairFunction = [TrainingInput] -> [TrainingInput] -> PairedInputs
