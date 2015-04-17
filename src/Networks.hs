module Networks where

import Types

-- A perceptron can be readily produced by partially applying it to a list of weights and an activation function, which can then be applied to any input to produce a classification
perceptron :: ActivationFunction -> Weights -> Network
perceptron act ws = act . perceptron' ws

-- Compute the value returned by the network, sans the activation function
perceptron' :: Weights -> Input -> Double
perceptron' [w] [] = w
perceptron' (w:ws) (x:xs) = w * x + perceptron' ws xs

-- The sign function
sign :: ActivationFunction
sign = signum

-- The following network combinations, for now, assume sign as the activation function

-- Complement
complementNet :: Network -> Network
complementNet n = negate . n

-- Intersection
intersectNet :: Network -> Network -> Network
n1 `intersectNet` n2 = \xs -> sign $ (n1 xs) + (n2 xs) - 0.5

-- Union
unionNet :: Network -> Network -> Network
n1 `unionNet` n2 = \xs -> sign $ (n1 xs) + (n2 xs) + 0.5

-- A separating hyperplane
-- u is assumed to be classed as -1, v as +1 and c is a parameter used to determine
-- where the hyperplane should be positioned between the two
sep :: Input -> Input -> Double -> Input -> Double
sep u v c = \x -> (x <.> w) - l
    where
      squaredNorm = sum . map (^2)
      a <.> b = sum $ zipWith (*) a b
      w = zipWith (-) v u
      l = c * (squaredNorm w) - (squaredNorm u) + (u <.> v)

-- A basic separating perceptron
-- The "spacing value" of c = 0.5 is hardcoded for this version
perc :: Input -> Input -> Network
perc plusOne minusOne = sign . sep minusOne plusOne 0.5

