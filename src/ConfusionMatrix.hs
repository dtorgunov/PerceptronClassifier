{- |
Module      : $Header$
Description : Definition of 'ConfusionMatrix' and related functions
Copyright   : (c) Denis Torgunov, 2015-2016
License     : No license

Maintainer  : dtorgunov@leafonthewind.net
Stability   : experimental
Portability : portable (depends on Gtk2Hs)

This module provides the definitions of the 'ConfusionMatrix' data type, and all related data types and functions, such as those used to construct it and average them.
-}
module ConfusionMatrix (
                        ConfusionMatrix
                       , truePositives
                       , trueNegatives
                       , falsePositives
                       , falseNegatives
                       , accuracy
                       , emptyConfusionMatrix
                       , createConfusionMatrix
                       , averageConfusionMatrix
                       ) where

import Types


-- | A confusion matrix holds the count of true/false positives/negatives and an accuracy measure.
data ConfusionMatrix = ConfusionMatrix { truePositives :: Double
                                       , trueNegatives :: Double
                                       , falsePositives :: Double
                                       , falseNegatives :: Double
                                       , accuracy :: Double
                                       }


-- * Those are the predicates used to build confusion matrices. Given a tuple (returned, expected)
-- where returned is the classification, as given by the network, and expected is the 'correct' one
-- it checks whether a point is a true positive, true negative, etc.

truePositivesP :: (Classification, Classification) -> Bool
truePositivesP (returned, expected) = (returned == 1) && (expected == 1)
trueNegativesP :: (Classification, Classification) -> Bool
trueNegativesP (returned, expected) = (returned == (-1)) && (expected == (-1))
falsePositivesP :: (Classification, Classification) -> Bool
falsePositivesP (returned, expected) = (returned == 1) && (expected == (-1))
falseNegativesP :: (Classification, Classification) -> Bool
falseNegativesP (returned, expected) = (returned == (-1)) && (expected == 1)

-- * Those functions utilise the predicates above to count the amount of each type of point,
-- and lift the resulting length to a 'Double'.

countTruePositives :: [(Classification, Classification)] -> Double
countTruePositives = fromIntegral . length . filter truePositivesP
countTrueNegatives :: [(Classification, Classification)] -> Double
countTrueNegatives = fromIntegral . length . filter trueNegativesP
countFalsePositives :: [(Classification, Classification)] -> Double
countFalsePositives = fromIntegral . length . filter falsePositivesP
countFalseNegatives :: [(Classification, Classification)] -> Double
countFalseNegatives = fromIntegral . length . filter falseNegativesP

-- | Computes the accuracy of classification.
accuracyMeasure :: [(Classification, Classification)] -> Double
accuracyMeasure results = (truePositives + trueNegatives)*100.0
                          /(fromIntegral total)
    where
      truePositives = countTruePositives results
      trueNegatives = countTrueNegatives results
      total = length results

-- | An empty 'ConfusionMatrix'. Needed since the data constructor is not exported,
-- in case a "default" matrix is needed.
emptyConfusionMatrix :: ConfusionMatrix
emptyConfusionMatrix = ConfusionMatrix 0 0 0 0 0

-- | Creates a new 'ConfusionMatrix' from given test results.
createConfusionMatrix :: [(Classification, Classification)] -> ConfusionMatrix
createConfusionMatrix results = ConfusionMatrix { truePositives = countTruePositives results
                                                , trueNegatives = countTrueNegatives results
                                                , falsePositives = countFalsePositives results
                                                , falseNegatives = countFalseNegatives results
                                                , accuracy = accuracyMeasure results
                                                }

-- | The arithmetic mean of a list.
mean :: [Double] -> Double
mean l = (sum l)
         / (fromIntegral (length l))

-- | Given a field accessor and a list of matrices, mean of that field for all matrices.
meanField :: (ConfusionMatrix -> Double) -> [ConfusionMatrix] -> Double
meanField accsessor cms = mean $ map accsessor cms

-- | Given a list of matrices, construct a matrix with each field being the average of the values of
-- the corresponding filed for all matrices in the list.
averageConfusionMatrix :: [ConfusionMatrix] -> ConfusionMatrix
averageConfusionMatrix cms = ConfusionMatrix { truePositives = meanField truePositives cms
                                             , trueNegatives = meanField trueNegatives cms
                                             , falsePositives = meanField falsePositives cms
                                             , falseNegatives = meanField falseNegatives cms
                                             , accuracy = meanField accuracy cms
                                             }
