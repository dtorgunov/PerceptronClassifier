module Training where

import Data.List    
import Data.Function
import Types
import Inputs

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
