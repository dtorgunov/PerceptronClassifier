{- |
Module      : $Header$
Description : The training algorithm implementation
Copyright   : (c) Denis Torgunov, 2015-2016
License     : No license

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

-- Should add early filtering -- adding this

distance :: Input -> Input -> Double
distance x y = sqrt $ sum $ map (^2) $ zipWith (-) x y

-- Sorts the training set by distance to the first argument
sortByDistanceTo :: Input -> TrainingSet -> TrainingSet
sortByDistanceTo x = sortBy distanceFunction
    where
      distanceFunction :: TrainingInput -> TrainingInput -> Ordering
      distanceFunction y1 y2 = compare (distance x (fst y1)) (distance x (fst y2))

seave :: TrainingSet -> Network -> TrainingSet
seave ts net = filter misclassified ts
    where
      misclassified :: TrainingInput -> Bool
      misclassified (i, c) = (runNetwork net i) /= c

plusPoints = filter (\(x,c) -> c == 1)
minusPoints = filter (\(x, c) -> c == (-1))

augmentNetwork :: Network -> Network -> TrainingInput -> TrainingSet -> Network
augmentNetwork outerNet n (plusOne, _) []
    = n
augmentNetwork outerNet n po@(plusOne, _) ((minusOne, _):minusOnes)
    = augmentNetwork outerNet newNet po (seave minusOnes (n `unionNet` newNet))
    where
      newNet = n `intersectNet` (hyperplane plusOne minusOne 0.5)

createNetwork' :: Network -> TrainingSet -> TrainingSet -> TrainingSet -> Network
createNetwork' n ts [] _ = n -- out of plus ones -- maybe remove this? should be captured below?
createNetwork' n ts plusOnes minusOnes
    | null $ seave ts n = n -- early stopping!
createNetwork' n ts po@(plusOne:plusOnes) minusOnes
    = createNetwork' newNet ts plusOnes minusOnes
    where
      newNet = n `unionNet` (augmentNetwork n emptyNet plusOne (sortByDistanceTo (fst plusOne) minusOnes)) -- consider adding a seave here. also, replace emptyNet with something better!

createNetwork :: SeparatorFunction -> TrainingSet -> Network
createNetwork sf ts = createNetwork' startingNet ts plusOnes minusOnes
    where
      plusOnes = plusPoints ts
      minusOnes = minusPoints ts
      startingNet = sf plusOnes minusOnes
