{- |
Module      : $Header$
Description : Types common to most modules in the project
Copyright   : (c) Denis Torgunov, 2015-2016
License     : No license

Maintainer  : dtorgunov@leafonthewind.net
Stability   : experimental
Portability : portable (depends on Gtk2Hs)

This module hosts all of the types that are used throughout the project, restricted to simple type aliases.
Data types are usually defined together with their supporting functions in other modules.
-}
module Types where

import Data.Tree

-- | The 'Weights' type is a collection of weights assigned to a perceptron.
type Weights = [Double]

-- | 'Input' is a position vector (represtnted as a list of 'Double's) of a point that is to be classified.
type Input = [Double]

-- | A 'Classification', despite being a 'Double', is actually restricted to 2 values in its use: +1.0 and -1.0.
-- Its definition is subject to change, to better reflect this convention.
type Classification = Double

-- | A function to convert from an arbitrary result of applying 'Weights' to an 'Input' to a 'Classification'. Currently, the sign function is used exclusively.
type ActivationFunction = Double -> Classification

-- | A tree representing a network of perceptrons
type PerceptronNetwork = Tree Weights
    
-- | 'TrainingInput' represents any input that is used during the training process, whether for construction of validation
-- of the network. It is an 'Input' vector, paired with the correct 'Classification' of a point.
type TrainingInput = (Input, Classification)

-- | A 'TrainingSet' can be used during either construction or validation.
type TrainingSet = [TrainingInput]

-- | 'PairedInputs' represent pairs of +1 and -1 points that will be used during network construction to construct separating
-- hyperplanes. It is a list of lists, with each list being a list of -1 points paired to a specific +1 point.
-- Used in the early algorithm versions.
type PairedInputs = [[(TrainingInput, TrainingInput)]]

-- | A function that determined the order in which the points will be compared.
type PairFunction = [TrainingInput] -> [TrainingInput] -> PairedInputs


-- | A simple mapping from 'String's to 'Double's,
-- produced when parsing, converting the 'String' classes to +1.0 or -1.0
type ClassMap = [(String, Double)] 
