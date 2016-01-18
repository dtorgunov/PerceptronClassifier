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


data ConfusionMatrix = ConfusionMatrix { truePositives :: Double
                                       , trueNegatives :: Double
                                       , falsePositives :: Double
                                       , falseNegatives :: Double
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

countTruePositives :: [(Classification, Classification)] -> Double
countTruePositives = fromIntegral . length . filter truePositivesP
countTrueNegatives :: [(Classification, Classification)] -> Double
countTrueNegatives = fromIntegral . length . filter trueNegativesP
countFalsePositives :: [(Classification, Classification)] -> Double
countFalsePositives = fromIntegral . length . filter falsePositivesP
countFalseNegatives :: [(Classification, Classification)] -> Double
countFalseNegatives = fromIntegral . length . filter falseNegativesP

accuracyMeasure :: [(Classification, Classification)] -> Double
accuracyMeasure results = (truePositives + trueNegatives)*100.0
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

mean :: [Double] -> Double
mean l = (sum l)
         / (fromIntegral (length l))

meanField :: (ConfusionMatrix -> Double) -> [ConfusionMatrix] -> Double
meanField accsessor cms = mean $ map accsessor cms

averageConfusionMatrix :: [ConfusionMatrix] -> ConfusionMatrix
averageConfusionMatrix cms = ConfusionMatrix { truePositives = meanField truePositives cms
                                             , trueNegatives = meanField trueNegatives cms
                                             , falsePositives = meanField falsePositives cms
                                             , falseNegatives = meanField falseNegatives cms
                                             , accuracy = meanField accuracy cms
                                             }
