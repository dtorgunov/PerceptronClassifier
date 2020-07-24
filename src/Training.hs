{- |
Module      : $Header$
Description : The training algorithm implementation
Copyright   : (c) Denis Torgunov, 2015-2016
License     : MIT

Maintainer  : dtorgunov@leafonthewind.net
Stability   : experimental
Portability : portable (depends on Gtk2Hs)

This module defines a 'createNetwork' function which can construct/train a network. 
-}
module Training (
                 createNetwork
                )where

import Data.List    
import Data.Function

import Types
import Networks
import InitialSeparators

-- | Compute the Euclidean distance between 2 points
distance :: Input -> Input -> Double
distance x y = sqrt $ sum $ map (^2) $ zipWith (-) x y

-- | Sort the training set by distance to the first argument
sortByDistanceTo :: Input -> TrainingSet -> TrainingSet
sortByDistanceTo x = sortBy distanceFunction
    where
      distanceFunction :: TrainingInput -> TrainingInput -> Ordering
      distanceFunction y1 y2 = compare (distance x (fst y1)) (distance x (fst y2))

-- | Filter the 'TrainingSet' so that only points misclassified by 'Network'
-- remain
seave :: TrainingSet -> Network -> TrainingSet
seave ts net = filter misclassified ts
    where
      misclassified :: TrainingInput -> Bool
      misclassified (i, c) = (runNetwork net i) /= c

-- | Filter, keeping only the points that should be classified as +1
plusPoints :: [TrainingInput] -> [TrainingInput]
plusPoints = filter (\(x,c) -> c == 1)
-- | Filter, keeping only the points that should be classified as -1
minusPoints :: [TrainingInput] -> [TrainingInput]
minusPoints = filter (\(x, c) -> c == (-1))

-- | Construct a network that will correctly classify several -1 points in relation to a +1 point
augmentNetwork :: Network -- ^ the outer network -- the one the network constructed will be united with
               -> Network -- ^ the accumulator for the network being constructed
               -> TrainingInput -- ^ a +1 point
               -> TrainingSet -- ^ (misclassified) -1 points
               -> Network
augmentNetwork outerNet n (plusOne, _) []
    = n
augmentNetwork outerNet n po@(plusOne, _) ((minusOne, _):minusOnes)
    = augmentNetwork outerNet newNet po (seave minusOnes (n `unionNet` newNet))
    where
      newNet = n `intersectNet` (hyperplane plusOne minusOne 0.5)

-- | The main recursive function called by 'createNetwork'
createNetwork' :: Network -> TrainingSet -> TrainingSet -> TrainingSet -> Network
createNetwork' n ts [] _ = n 
createNetwork' n ts plusOnes minusOnes
    | null $ seave ts n = n -- early stopping!
createNetwork' n ts po@(plusOne:plusOnes) minusOnes
    = createNetwork' newNet ts plusOnes minusOnes
    where
      newNet = n `unionNet` (augmentNetwork n emptyNet plusOne (sortByDistanceTo (fst plusOne) minusOnes)) -- consider adding a seave here in future iterations

-- | Construct a classifying neural network
createNetwork :: SeparatorFunction -- ^ a function to determine the initial separator
              -> TrainingSet -- ^ a set of training inputs
              -> Network
createNetwork sf ts = createNetwork' startingNet ts plusOnes minusOnes
    where
      plusOnes = plusPoints ts
      minusOnes = minusPoints ts
      startingNet = sf plusOnes minusOnes
