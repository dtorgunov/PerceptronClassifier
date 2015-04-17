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


-- Checks all ys against the first x, then all ys against the second x, and so on
pairLists :: PairFunction
pairLists xs ys = map pairWithYs xs
    where
      pairWithYs :: TrainingInput -> [(TrainingInput, TrainingInput)]
      pairWithYs x = (repeat x) `zip` ys
