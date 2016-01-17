module Validation where

import Types
import Networks
import System.Random
import Data.List


data ConfusionMatrixData = NoData |  ConfusionMatrixData { tp :: Int
                                                         , tn :: Int
                                                         , fp :: Int
                                                         , fn :: Int
                                                         , acc :: Double
                                                         }

-- Take n unique elements from a given list
takeUnique :: (Eq a) => Int -> [a] -> [a] -> [a]
takeUnique 0 _ acc = acc
takeUnique n (i:ins) acc | i `elem` acc = takeUnique n ins acc
                         | otherwise = takeUnique (n-1) ins (i:acc)

-- Take the elements at indexes in is from the list of xs
takeIndexes :: [Int] -> [a] -> [a] -> [a]
takeIndexes [] _ acc = acc
takeIndexes (i:is) xs acc = takeIndexes is xs ((xs !! i):acc)
                                       

-- A function to fascilitate verification.
-- Given a list, split it roughly p/(100 - p)
splitList' :: (RandomGen g, Eq a) => Int -> g -> [a] -> ([a], [a])
splitList' 100 _ xs = (xs, xs)
splitList' p g xs = (training, verification)
    where
      indexes = takeUnique ((length xs - 1) * p `div` 100) (randomRs (0, (length xs - 1)) g) []
      training = takeIndexes indexes xs []
      verification = xs \\ training

accuracyMeasure :: Int -> Int -> Int -> Double
accuracyMeasure truePositives trueNegatives total
    = (fromIntegral (truePositives+trueNegatives))*100.0/ (fromIntegral total)

confusionMatrix :: TrainingSet -> Network -> ConfusionMatrixData
confusionMatrix ts net = let results = map (\(x,y) -> ((runNetwork net) x, y)) ts
                             truePositives = length $ filter (\(x,y) -> (x == 1) && (y == 1)) results
                             trueNegatives = length $ filter (\(x,y) -> (x == (-1)) && (y == (-1))) results
                             falsePositives = length $ filter (\(returned, expected) -> ((returned == 1)
                                                                                        && (expected == (-1)))) results
                             falseNegatives = length $ filter (\(x,y) -> (x== (-1)) && (y == 1)) results
                             accuracy = accuracyMeasure truePositives trueNegatives (length results)
                         in ConfusionMatrixData { tp = truePositives, tn = trueNegatives
                                                , fp = falsePositives, fn = falseNegatives
                                                , acc = accuracy
                                                }
                         
-- Use p% of the set for training, and (100-p)% for one-time validation
splitValidation :: (RandomGen g) => Int -> g -> TrainingSet -> (TrainingSet -> Network) -> (ConfusionMatrixData, ConfusionMatrixData, Network)
splitValidation p gen dataSet trainingMethod = (cmdTraining, cmdValidation, network)
    where
      (training, validation) = splitList' p gen dataSet
      network = trainingMethod training
      cmdTraining = confusionMatrix training network
      cmdValidation = confusionMatrix validation network
      
