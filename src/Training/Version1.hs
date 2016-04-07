{- |
Module      : $Header$
Description : The initial training algorithm implementation
Copyright   : (c) Denis Torgunov, 2015-2016
License     : No license

Maintainer  : dtorgunov@leafonthewind.net
Stability   : experimental
Portability : portable (depends on Gtk2Hs)

Like all 'Training' modules, this one defines a 'createNetwork' function which can construct/train a network. This module implements the simplest possible interpretation of the algorithm. See 'Training.Version2' for a more sophisticated approach.
-}
module Training.Version1 (
                          createNetwork
                         )where

import Data.List    
import Data.Function

import Types
import Networks
import InitialSeparators

-- | Given a 'PairFunction' and a 'TrainingSet', pair the inputs according to the pair function.
pairInputs :: PairFunction -> TrainingSet -> PairedInputs
pairInputs f is = let [xs, ys] = groupInputs is
                  in f ys xs


-- | Sort and group a training set based on the 'snd' element.
groupInputs :: TrainingSet -> [[TrainingInput]]
groupInputs = groupBy ((==) `on` snd) . sortBy (compare `on` snd)


-- | Compute the distance between 2 points.
distance :: Input -> Input -> Double
distance x y = sqrt $ sum $ map (^2) $ zipWith (-) x y


-- | Pair all -1s with the first +1, then all -1s with the second +1, and so on.
pairLists :: PairFunction
pairLists xs ys = map pairWithYs xs
    where
      pairWithYs :: TrainingInput -> [(TrainingInput, TrainingInput)]
      pairWithYs x = (repeat x) `zip` ys


-- | The extension of 'pairLists' that sorts the points by distance before pairing them.
distanceBased :: PairFunction
distanceBased xs ys = map (sortBy distanceCompare) $ pairLists xs ys
    where
      -- Could be written more cleanly. Also, due to the way pairLists is defined, the first element of both tuples will be identical
      distanceCompare :: (TrainingInput, TrainingInput) -> (TrainingInput, TrainingInput) -> Ordering
      distanceCompare (x, y1) (_, y2) = distance (fst x) (fst y1) `compare` distance (fst x) (fst y2)


-- | Given a list of +1/-1 point pairs with an identical +1 point (such as those returned by 'distanceBased'), construct a tuple with the +1 point and a list of -1 points.
simplify :: [(TrainingInput, TrainingInput)] -> (TrainingInput, [TrainingInput])
simplify ps@((x,_):_) = (x, map snd ps)

-- | Given a 'Network' and a set of points, only leave the points which are not classified by the network correctly.
seave :: Network -> (TrainingInput, [TrainingInput]) -> (TrainingInput, [TrainingInput])
seave net (x,ys) = (x, filter misclassified ys)
    where
      misclassified :: TrainingInput -> Bool
      misclassified (point, c) = ((runNetwork net) point) /= c

-- | Given a 'Network' and a list of points misclassified by it, add a new perceptron to
-- classify the first point, and then filter out the points now classified correctly before
-- recursing, eventually yielding a network that classified all points correctly.
augmentNetwork :: Network -> (TrainingInput, [TrainingInput]) -> Network
augmentNetwork net (_, []) = net
augmentNetwork net (x, (y:ys)) = augmentNetwork newNet (seave newNet (x, ys))
    where
      newNet = net `intersectNet` (hyperplane (fst x) (fst y) 0.5) -- c = 0.5 hardcoded

-- | Create a network that correctly classifies all -1 points in relation to a single +1 point.
createPlusNet :: Network -> [(TrainingInput, TrainingInput)] -> Network
createPlusNet sepNet tis = augmentNetwork net (seave net simplifiedTis)
    where
      simplifiedTis = simplify tis
      netNoSep = hyperplane (fst $ fst simplifiedTis) (fst $ head $ snd simplifiedTis) 0.5 -- c = 0.5
      net = if (isEmptyNet sepNet) then netNoSep else sepNet

-- | From a list of networks for each +1 point, create a network that classifies all of them correctly.
unifyNetwork :: [Network] -> Network
unifyNetwork (net:nets) = foldr unionNet net nets

-- | Create a network, given a training set.
createNetwork :: SeparatorFunction -> TrainingSet -> Network
createNetwork sf ts = unifyNetwork $ map (createPlusNet sepNet) preparedInputs
    where
      preparedInputs = pairInputs distanceBased ts
      plusOnes = filter (\(x,c) -> c == 1) ts
      minusOnes = filter (\(x,c) -> c == (-1)) ts
      sepNet = sf plusOnes minusOnes
