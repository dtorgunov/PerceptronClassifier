module Types where

type Weights = [Double]
type Input = [Double]
type Classification = Double
type ActivationFunction = Double -> Classification
type Network = Input -> Classification

type TrainingInput = (Input, Classification)
type TrainingSet = [TrainingInput]
type PairedInputs = [[(TrainingInput, TrainingInput)]]

-- A function that will determine the order in which the points will be compaired
-- against one another
type PairFunction = [TrainingInput] -> [TrainingInput] -> PairedInputs


