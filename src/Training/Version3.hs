module Training.Version3 (
                          createNetwork
                         )where

import Data.List    
import Data.Function
import Types
import Networks
import InitialSeparators

pairInputs :: PairFunction -> TrainingSet -> PairedInputs
pairInputs f is = let [xs, ys] = groupInputs is
                  in f ys xs


groupInputs :: TrainingSet -> [[TrainingInput]]
groupInputs = groupBy ((==) `on` snd) . sortBy (compare `on` snd)


distance :: Input -> Input -> Double
distance x y = sqrt $ sum $ map (^2) $ zipWith (-) x y


-- Checks all ys against the first x, then all ys against the second x, and so on
pairLists :: PairFunction
pairLists xs ys = map pairWithYs xs
    where
      pairWithYs :: TrainingInput -> [(TrainingInput, TrainingInput)]
      pairWithYs x = (repeat x) `zip` ys


-- The extension of the function above, with the individual lists sorted by distance,
-- closest to farthest
distanceBased :: PairFunction
distanceBased xs ys = map (sortBy distanceCompare) $ pairLists xs ys
    where
      -- Could be written more cleanly. Also, due to the way pairLists is defined, the first element of both tuples will be identical
      distanceCompare :: (TrainingInput, TrainingInput) -> (TrainingInput, TrainingInput) -> Ordering
      distanceCompare (x, y1) (_, y2) = distance (fst x) (fst y1) `compare` distance (fst x) (fst y2)


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
createNetwork' :: TrainingSet -> Network -> Network
createNetwork' ts net
    | isEmptyNet net = createNetwork' ts (createPlusNet plusPoint minusPoints)
    | seaved == [] = net
    | otherwise = createNetwork' ts (net `unionNet` (createPlusNet plusPoint minusPoints))
    where
      seaved = seave ts net
      plusPoints' = plusOnes seaved
      minusPoints' = minusOnes seaved
      plusPoint = if (plusOnes seaved) == [] then head $ plusOnes ts else head $ plusOnes seaved
      minusPoints = if (minusOnes seaved) == [] then (sortByDistanceTo $ fst plusPoint) $ minusOnes ts else (sortByDistanceTo $ fst plusPoint) $ minusOnes seaved
      -- use centres of mass?
      -- plusPoint = if plusPoints' == [] then cgPlusOne else head $ plusOnes
      -- minusPoints = if minusPoints' == [] then [cgMinusOne] else (sortByDistanceTo $ fst plusPoint) $ minusOnes seaved

createNetwork :: SeparatorFunction -> TrainingSet -> Network
createNetwork sf ts = createNetwork' ts (sf plusPoints minusPoints)
    where
      plusPoints = plusOnes ts
      minusPoints = minusOnes ts

plusOnes = filter (\(x,c) -> c == 1)
minusOnes = filter (\(x, c) -> c == (-1))
