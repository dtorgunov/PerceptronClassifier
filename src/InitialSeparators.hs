{- |
Module      : $Header$
Description : A module for constructing a simple separator that is used as the starting point for the overall network construction
Copyright   : (c) Denis Torgunov, 2015-2016
License     : No license

Maintainer  : dtorgunov@leafonthewind.net
Stability   : experimental
Portability : portable (depends on Gtk2Hs)

This module defines a 'SeparatorFunction' type, and exports 2 simple separator functions: 'noSeparator' and 'centroidSeparator'.
-}
module InitialSeparators (
                          SeparatorFunction
                         , noSeparator
                         , centroidSeparator
                         ) where

import Types
import Networks
import Data.List

-- | A function that creates a 'Network' separating the two training sets given. The 'Network' should use some form of metric gained from the points as a whole.
type SeparatorFunction = [TrainingInput] -- ^ All of the +1 points
                       -> [TrainingInput] -- ^ All of the -1 points
                       -> Network

-- | A dummy separator that mimics the absence of one, returning an empty network regardless of inputs.
noSeparator :: SeparatorFunction
noSeparator _ _ = emptyNet

-- | Out of a list of points, construct a new "point", the coordinates of which are sums of the corresponding coordinates across the input lists.
sumsOfCoordinates :: [Input] -> Input
sumsOfCoordinates = map sum . transpose

-- | Determine the centre of gravity of a set of points.
centroid :: [Input] -> Input
centroid ps = map (/ numOfSamples) (sumsOfCoordinates ps)
    where
      numOfSamples = fromIntegral $ length ps

-- | A separator based on the points' respective centres of gravity.
centroidSeparator :: SeparatorFunction
centroidSeparator plusOnes minusOnes = hyperplane (centroid plusOnes') (centroid minusOnes') 0.5
    where
      plusOnes' = map fst plusOnes
      minusOnes' = map fst minusOnes
