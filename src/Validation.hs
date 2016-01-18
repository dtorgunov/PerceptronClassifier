module Validation (
                   ValidationFunction
                  , splitValidation
                  , crossValidation
                  )where
    
import Types
import Networks
import System.Random
import Data.List
import ConfusionMatrix

type ValidationFunction = StdGen -> TrainingSet -> (TrainingSet -> Network) -> (ConfusionMatrix, ConfusionMatrix, Network)

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
splitValidation :: Int -> ValidationFunction
splitValidation p gen dataSet trainingMethod = (cmdTraining, cmdValidation, network)
    where
      (training, validation) = splitList p gen dataSet
      network = trainingMethod training
      cmdTraining = confusionMatrix training network
      cmdValidation = confusionMatrix validation network


-- Splits a list into k (approximately) equal-sized sublists.
equalLengthSublists :: Int -> [a] -> [[a]]
equalLengthSublists k l
    | (length l) `mod` k == 0 = equalLengthSublistsEven k l
    | otherwise = equalLengthSublistsUneven k l

equalLengthSublistsUneven :: Int -> [a] -> [[a]]
equalLengthSublistsUneven k l = splitBy k' firstPart ++ splitBy k'' secondPart
    where
      sampleSize = length l
      k' = (sampleSize `div` k) + 1
      k'' = sampleSize `div` k
      firstPart = take ((sampleSize `mod` k) * k') l
      secondPart = drop ((sampleSize `mod` k) * k') l

-- This is used when the number of samples is evenly divisible by k
equalLengthSublistsEven :: Int -> [a] -> [[a]]
equalLengthSublistsEven k l = splitBy k' l
    where
      k' = (length l) `div` k

-- splitBy :: Int -> [a] -> [[a]]
-- splitBy k = takeWhile (not . null) . unfoldr (Just . splitAt k)
splitBy' :: Int -> [a] -> [[a]] -> [[a]]
splitBy' k [] acc = reverse acc
splitBy' k s acc = splitBy' k (drop k s) ((take k s):acc)

splitBy :: Int -> [a] -> [[a]]
splitBy k s = splitBy' k s []

folds :: (RandomGen g) => Int -> g -> TrainingSet -> [TrainingSet]
folds k g = equalLengthSublists k . shuffleList g

validateOnEach' :: Int -> [(ConfusionMatrix, ConfusionMatrix)] -> (TrainingSet -> Network) -> [TrainingSet] -> [(ConfusionMatrix, ConfusionMatrix)]
validateOnEach' n acc trainingMethod dataSets
    | n >= (length dataSets) = reverse acc
    | otherwise = validateOnEach' (n+1) ((trainingMat, validMat):acc) trainingMethod dataSets
    where
      validationSet = dataSets !! n
      trainingSet = concat $ (take n dataSets) ++ (drop (n+1) dataSets)
      network = trainingMethod trainingSet
      trainingMat = confusionMatrix trainingSet network
      validMat = confusionMatrix validationSet network
                    

validateOnEach :: (TrainingSet -> Network) -> [TrainingSet] -> [(ConfusionMatrix, ConfusionMatrix)]
validateOnEach = validateOnEach' 0 []

crossValidation :: Int -> ValidationFunction
crossValidation k g dataSet trainingMethod = let dataSets = folds k g dataSet
                                                 matrices = validateOnEach trainingMethod dataSets
                                                 finalNetwork = trainingMethod dataSet
                                                 trainAvgMat = averageConfusionMatrix $ map fst matrices
                                                 validAvgMat = averageConfusionMatrix $ map snd matrices
                                             in (trainAvgMat, validAvgMat, finalNetwork)
