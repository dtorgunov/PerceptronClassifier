module InitialSeparators where

import Types
import Networks
import Data.List

noSeparator :: SeparatorFunction
noSeparator _ _ = (makeNetwork Empty)

-- Given a list of points, return a point where x_1 is the sum of all x_1s in the list, x_2 is the sum of all x_2s, etc
sumsOfCoordinates :: [Input] -> Input
sumsOfCoordinates = map sum . transpose

-- The centre of gravity of a set of points
centroid :: [Input] -> Input
centroid ps = map (/ numOfSamples) (sumsOfCoordinates ps)
    where
      numOfSamples = fromIntegral $ length ps

centroidSeparator :: SeparatorFunction
centroidSeparator plusOnes minusOnes = makeNetwork $ Hyperplane (centroid plusOnes') (centroid minusOnes') 0.5
    where
      plusOnes' = map fst plusOnes
      minusOnes' = map fst minusOnes
