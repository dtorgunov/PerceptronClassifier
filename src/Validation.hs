{- |
Module      : $Header$
Description : Definition of validation functions
Copyright   : (c) Denis Torgunov, 2015-2016
License     : MIT

Maintainer  : dtorgunov@leafonthewind.net
Stability   : experimental
Portability : portable (depends on Gtk2Hs)

This module deals with validating the correctness of the model generated by the algorithm.
-}
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

-- | A type represting a validation function. Note that it accepts a random seed in order to keep it pure and the results
-- reproducable, if needed.
type ValidationFunction = StdGen -> TrainingSet -> (TrainingSet -> Network) -> (ConfusionMatrix, ConfusionMatrix, Network)

-- | The accumulator-based implementation of 'shuffleList'
shuffleList' :: (RandomGen g, Eq a) => g -> [a] -> [a] -> [a]
shuffleList' _ [] accum = accum
shuffleList' g ss accum = shuffleList' g' ss' (s:accum)
    where
      (index, g') = randomR (0, (length ss - 1)) g -- determine a random index within the list
      s = ss !! index -- get element at index
      ss' = (take index ss) ++ (drop (index+1) ss) -- retain all elements other than the one at index

-- | Given a random seed and a list, reorganise the list randomly.
shuffleList :: (RandomGen g, Eq a) => g -> [a] -> [a]
shuffleList g l = shuffleList' g l []


-- | Shuffle and split a given list, with p% of its contents ending up in the first list, and the rest in the second
-- Passing 100 as p makes both lists equal.
splitList :: (RandomGen g, Eq a) => Int -> g -> [a] -> ([a], [a])
splitList 100 _ xs = (xs, xs)
splitList p g xs = (training, verification)
    where
      shuffledList = shuffleList g xs
      trainingElements = ((length xs) * p `div` 100)
      training = take trainingElements shuffledList
      verification = drop trainingElements shuffledList

-- | Given a 'TrainingSet' (either the one used in training, or one used for validation), and a trained network,
-- determine the confusion matrix to display to the user.
confusionMatrix :: TrainingSet -> Network -> ConfusionMatrix
confusionMatrix ts net = let results = map (\(x,y) -> ((runNetwork net) x, y)) ts
                         in createConfusionMatrix results 

-- | Split a (shuffled) training set into p% and (100-p)%, using the first for training and the second for validation.
-- This function is meant to be partially applied to p (the percentage), yielding a 'ValidationFunction'.
splitValidation :: Int -> ValidationFunction
splitValidation p gen dataSet trainingMethod = (cmdTraining, cmdValidation, network)
    where
      (training, validation) = splitList p gen dataSet
      network = trainingMethod training
      cmdTraining = confusionMatrix training network
      cmdValidation = confusionMatrix validation network


-- | Split a list into k approxmiately equal-sized sublists. If the list is not divisible by k,
-- the first (length `mod` k) sublists will end up with 1 element more than the following sublists.
equalLengthSublists :: Int -> [a] -> [[a]]
equalLengthSublists k l = splitBy k' firstPart ++ splitBy k'' secondPart
    where
      sampleSize = length l
      k' = (sampleSize `div` k) + 1
      k'' = sampleSize `div` k
      firstPart = take ((sampleSize `mod` k) * k') l
      secondPart = drop ((sampleSize `mod` k) * k') l

-- | An accumulator-based recursive function called by 'splitBy'
splitBy' :: Int -> [a] -> [[a]] -> [[a]]
splitBy' k [] acc = reverse acc
splitBy' k s acc = splitBy' k (drop k s) ((take k s):acc)

-- | Split a list into sublists of a given size
splitBy :: Int -> [a] -> [[a]]
splitBy k s = splitBy' k s []

-- | Prepares k folds to be used for k-fold cross-validation
folds :: (RandomGen g) => Int -> g -> TrainingSet -> [TrainingSet]
folds k g = equalLengthSublists k . shuffleList g

-- | An accumulator-based recursive function called by 'validateOnEach'
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
                    

-- | Given a training function and a list of 'TrainingSet's, construct
-- the training and validation confusion matrices, using each 'TrainingSet' for validation
-- exactly once
validateOnEach :: (TrainingSet -> Network) -> [TrainingSet] -> [(ConfusionMatrix, ConfusionMatrix)]
validateOnEach = validateOnEach' 0 []

-- | Construct a 'ValidationFunction' for k-fold cross-validation
crossValidation :: Int -- ^ the parametere k, determining the number of folds
                -> ValidationFunction
crossValidation k g dataSet trainingMethod = let dataSets = folds k g dataSet
                                                 matrices = validateOnEach trainingMethod dataSets
                                                 finalNetwork = trainingMethod dataSet
                                                 trainAvgMat = averageConfusionMatrix $ map fst matrices
                                                 validAvgMat = averageConfusionMatrix $ map snd matrices
                                             in (trainAvgMat, validAvgMat, finalNetwork)
