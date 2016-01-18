module Validation (
                   splitValidation
                  )where

import Types
import Networks
import System.Random
import Data.List
import ConfusionMatrix


shuffleList' :: (RandomGen g, Eq a) => g -> [a] -> [a] -> [a]
shuffleList' _ [] accum = accum
shuffleList' g ss accum = shuffleList' g' ss' (s:accum)
    where
      (index, g') = randomR (0, (length ss - 1)) g -- determine a random index within the list
      s = ss !! index -- get element at index
      ss' = (take index ss) ++ (drop (index+1) ss) -- retain all elements other than the one at index

shuffleList :: (RandomGen g, Eq a) => g -> [a] -> [a]
shuffleList g l = shuffleList' g l []

splitList :: (RandomGen g, Eq a) => Int -> g -> [a] -> ([a], [a])
splitList 100 _ xs = (xs, xs)
splitList p g xs = (training, verification)
    where
      shuffledList = shuffleList g xs
      trainingElements = ((length xs) * p `div` 100)
      training = take trainingElements shuffledList
      verification = drop trainingElements shuffledList


confusionMatrix :: TrainingSet -> Network -> ConfusionMatrix
confusionMatrix ts net = let results = map (\(x,y) -> ((runNetwork net) x, y)) ts
                         in createConfusionMatrix results 

-- Use p% of the set for training, and (100-p)% for one-time validation
splitValidation :: (RandomGen g) => Int -> g -> TrainingSet -> (TrainingSet -> Network) -> (ConfusionMatrix, ConfusionMatrix, Network)
splitValidation p gen dataSet trainingMethod = (cmdTraining, cmdValidation, network)
    where
      (training, validation) = splitList p gen dataSet
      network = trainingMethod training
      cmdTraining = confusionMatrix training network
      cmdValidation = confusionMatrix validation network
      
