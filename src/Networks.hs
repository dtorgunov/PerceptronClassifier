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
complement :: Network -> Network
complement n = negate . n

-- Intersection
intersect :: Network -> Network -> Network
n1 `intersect` n2 = \xs -> sign $ (n1 xs) + (n2 xs) - 0.5

-- Union
union :: Network -> Network -> Network
n1 `union` n2 = \xs -> sign $ (n1 xs) + (n2 xs) + 0.5




