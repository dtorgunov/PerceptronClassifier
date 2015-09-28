module Networks where

import Types

-- A perceptron can be readily produced by partially applying it to a list of weights and an activation function, which can then be applied to any input to produce a classification
-- perceptron :: ActivationFunction -> Weights -> Network
-- perceptron act ws = act . perceptron' ws

-- Compute the value returned by the network, sans the activation function
-- perceptron' :: Weights -> Input -> Double
-- perceptron' [w] [] = w
-- perceptron' (w:ws) (x:xs) = w * x + perceptron' ws xs

-- The sign function
sign :: ActivationFunction
sign = signum

-- The following network combinations, for now, assume sign as the activation function

-- A hyperplane separating plusOne from minusOne, with spacing c
hyperplane :: Input -> Input -> Double -> Network
hyperplane plusOne minusOne c = makeNetwork $ Hyperplane plusOne minusOne c

-- Intersection
intersectNet :: Network -> Network -> Network
n1 `intersectNet` n2 = makeNetwork $ Intersection (net n1) (net n2)

-- Union
unionNet :: Network -> Network -> Network
n1 `unionNet` n2 = makeNetwork $ Union (net n1) (net n2)

-- A separating hyperplane function
-- u is assumed to be classed as -1, v as +1 and c is a parameter used to determine
-- where the hyperplane should be positioned between the two
sepFunct :: Input -> Input -> Double -> Input -> Double
sepFunct u v c = \x -> (x <.> w) - l
    where
      squaredNorm = sum . map (^2)
      a <.> b = sum $ zipWith (*) a b
      w = zipWith (-) v u
      l = c * (squaredNorm w) - (squaredNorm u) + (u <.> v)

-- Generate a network function based on the network description
networkFunction :: NetworkDesc -> NetworkFunction
networkFunction (Hyperplane plusOne minusOne c) = sign . sepFunct minusOne plusOne c
networkFunction (Union n1 n2) = \xs -> sign $ (n1' xs) + (n2' xs) + 0.5
    where
      n1' = networkFunction n1
      n2' = networkFunction n2
networkFunction (Intersection n1 n2) = \xs -> sign $ (n1' xs) + (n2' xs) - 0.5
    where
      n1' = networkFunction n1
      n2' = networkFunction n2

-- Create a network based on the description of its topology
makeNetwork :: NetworkDesc -> Network
makeNetwork n = Network (networkFunction n) n

-- Classify a point Input by the Network given
runNetwork :: Network -> Input -> Classification
runNetwork n xs = (f n) xs

