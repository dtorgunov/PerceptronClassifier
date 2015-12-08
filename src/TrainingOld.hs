module TrainingOld where

import Data.List    
import Data.Function
import Types
import Networks

pairInputs :: PairFunction -> TrainingSet -> PairedInputs
pairInputs f is = let [xs, ys] = groupInputs is
                  in f ys xs


groupInputs :: TrainingSet -> [[TrainingInput]]
groupInputs = groupBy ((==) `on` snd) . sortBy (compare `on` snd)


distance :: Input -> Input -> Double
distance x y = sqrt $ sum $ map (^2) $ zipWith (-) x y


-- Checks all ys against the first x, then all ys against the second x, and so on
pairLists :: PairFunction
pairLists xs ys = map pairWithYs xs
    where
      pairWithYs :: TrainingInput -> [(TrainingInput, TrainingInput)]
      pairWithYs x = (repeat x) `zip` ys


-- The extension of the function above, with the individual lists sorted by distance,
-- closest to farthest
distanceBased :: PairFunction
distanceBased xs ys = map (sortBy distanceCompare) $ pairLists xs ys
    where
      -- Could be written more cleanly. Also, due to the way pairLists is defined, the first element of both tuples will be identical
      distanceCompare :: (TrainingInput, TrainingInput) -> (TrainingInput, TrainingInput) -> Ordering
      distanceCompare (x, y1) (_, y2) = distance (fst x) (fst y1) `compare` distance (fst x) (fst y2)


{-
  The following is a very basic training algorithm. 
  It can be further refined by taking into account the data from the networks previously
  created, tweaking certain parameters, and so on. Those refinements are subject to later
  development.
-}

-- Since we construct a network for each +1 point in turn, we restructure the originally paired training sets in a more managable fashion
-- I.e. a tuple, where the first element is the positive point, and the second element is
-- a list of the corresponding negative points
simplify :: [(TrainingInput, TrainingInput)] -> (TrainingInput, [TrainingInput])
simplify ps@((x,_):_) = (x, map snd ps)

-- Given a network and a set of points, only leave the points which are not classified by the network correctly
seave :: Network -> (TrainingInput, [TrainingInput]) -> (TrainingInput, [TrainingInput])
seave net (x,ys) = (x, filter misclassified ys)
    where
      misclassified :: TrainingInput -> Bool
      misclassified (point, c) = ((runNetwork net) point) /= c

-- Given a network and a list of points misclassified by it add a new perceptron to
-- classify the first point, and then filter out the points now classified correctly before
-- recursing, eventually yielding a network that classified all points correctly
augmentNetwork :: Network -> (TrainingInput, [TrainingInput]) -> Network
augmentNetwork net (x, []) = net
augmentNetwork net (x, (y:ys)) = augmentNetwork newNet (seave newNet (x, ys))
    where
      newNet = net `intersectNet` (hyperplane (fst x) (fst y) 0.5) -- c = 0.5 hardcoded

-- A starting point for the recursion above
createPlusNet :: [(TrainingInput, TrainingInput)] -> Network
createPlusNet tis = augmentNetwork net (seave net simplifiedTis)
    where
      simplifiedTis = simplify tis
      net = hyperplane (fst $ fst simplifiedTis) (fst $ head $ snd simplifiedTis) 0.5 -- c = 0.5

-- From a list of networks for each +1 point, create a network that classifies all of them correctly
unifyNetwork :: [Network] -> Network
unifyNetwork (net:nets) = foldr unionNet net nets

-- Create a network, given a training set
createNetwork :: TrainingSet -> Network
createNetwork ts = unifyNetwork $ map createPlusNet preparedInputs
    where
      preparedInputs = pairInputs distanceBased ts
