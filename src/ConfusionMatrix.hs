module ConfusionMatrix (
                        ConfusionMatrix
                       , truePositives
                       , trueNegatives
                       , falsePositives
                       , falseNegatives
                       , accuracy
                       , emptyConfusionMatrix
                       , createConfusionMatrix
                       ) where

import Types


data ConfusionMatrix = ConfusionMatrix { truePositives :: Int
                                       , trueNegatives :: Int
                                       , falsePositives :: Int
                                       , falseNegatives :: Int
                                       , accuracy :: Double
                                       }

-- Predicates for building a confusion matrix
truePositivesP :: (Classification, Classification) -> Bool
truePositivesP (returned, expected) = (returned == 1) && (expected == 1)
trueNegativesP :: (Classification, Classification) -> Bool
trueNegativesP (returned, expected) = (returned == (-1)) && (expected == (-1))
falsePositivesP :: (Classification, Classification) -> Bool
falsePositivesP (returned, expected) = (returned == 1) && (expected == (-1))
falseNegativesP :: (Classification, Classification) -> Bool
falseNegativesP (returned, expected) = (returned == (-1)) && (expected == 1)

countTruePositives :: [(Classification, Classification)] -> Int
countTruePositives = length . filter truePositivesP
countTrueNegatives :: [(Classification, Classification)] -> Int
countTrueNegatives = length . filter trueNegativesP
countFalsePositives :: [(Classification, Classification)] -> Int
countFalsePositives = length . filter falsePositivesP
countFalseNegatives :: [(Classification, Classification)] -> Int
countFalseNegatives = length . filter falseNegativesP

accuracyMeasure :: [(Classification, Classification)] -> Double
accuracyMeasure results = (fromIntegral (truePositives + trueNegatives))*100.0
                          /(fromIntegral total)
    where
      truePositives = countTruePositives results
      trueNegatives = countTrueNegatives results
      total = length results

emptyConfusionMatrix :: ConfusionMatrix
emptyConfusionMatrix = ConfusionMatrix 0 0 0 0 0

createConfusionMatrix :: [(Classification, Classification)] -> ConfusionMatrix
createConfusionMatrix results = ConfusionMatrix { truePositives = countTruePositives results
                                                , trueNegatives = countTrueNegatives results
                                                , falsePositives = countFalsePositives results
                                                , falseNegatives = countFalseNegatives results
                                                , accuracy = accuracyMeasure results
                                                }
