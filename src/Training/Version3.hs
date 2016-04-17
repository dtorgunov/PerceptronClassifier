module Training.Version3 (
                          createNetwork
                         )where

import Data.List    
import Data.Function
import Types
import Networks
import InitialSeparators

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
      misclassified (i, c) = (runNetwork net i) == c

augmentNetwork :: Network -> TrainingInput -> TrainingSet -> Network
augmentNetwork net plusPoint [] = net
augmentNetwork net plusPoint (minusPoint:minusPoints)
    = augmentNetwork newNet plusPoint (seave minusPoints newNet)
    where
      newNet = net `intersectNet` (hyperplane (fst plusPoint) (fst minusPoint) 0.5)

createPlusNet :: TrainingInput -> TrainingSet -> Network
createPlusNet plusPoint minusPoints = augmentNetwork net plusPoint (seave minusPoints net)
    where
      net = hyperplane (fst plusPoint) (fst $ head minusPoints) 0.5

-- much higher potential for overfitting due to the inclusion of non-seaved versions!
createNetwork' :: TrainingSet -> Network -> TrainingSet -> TrainingSet -> Network
createNetwork' ts net correctPlusOnes correctMinusOnes
    | isEmptyNet net = createNetwork' ts (createPlusNet plusPoint minusPoints) [] []
    | null seaved = net
    | otherwise = createNetwork' ts (net `unionNet` (createPlusNet plusPoint minusPoints)) correctPlusOnes' correctMinusOnes'
    where
      seaved = seave ts net
      plusPoints' = plusOnes seaved
      minusPoints' = minusOnes seaved
      correctPlusOnes' = undefined
      correctMinusOnes' = undefined
      -- use centres of mass?
      plusPoint = if plusPoints' == [] then undefined else head $ plusOnes seaved
      minusPoints = if minusPoints' == [] then undefined else (sortByDistanceTo $ fst plusPoint) $ minusOnes seaved
--      plusPoint = if (plusOnes seaved) == [] then head $ plusOnes ts else head $ plusOnes seaved
--      minusPoints = if (minusOnes seaved) == [] then (sortByDistanceTo $ fst plusPoint) $ minusOnes ts else (sortByDistanceTo $ fst plusPoint) $ minusOnes seaved

createNetwork :: SeparatorFunction -> TrainingSet -> Network
createNetwork sf ts = createNetwork' ts (sf plusPoints minusPoints) [] []
    where
      plusPoints = plusOnes ts
      minusPoints = minusOnes ts

plusOnes = filter (\(x,c) -> c == 1)
minusOnes = filter (\(x, c) -> c == (-1))
