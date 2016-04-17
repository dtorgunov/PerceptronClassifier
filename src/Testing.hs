{-# LANGUAGE ScopedTypeVariables #-}
import Test.QuickCheck
import Test.QuickCheck.Property as P
import Types
import Networks

-- Basic property of separating hyperplanes:

-- A separating hyperplane built to separate vectors u and v will always classify u and v correctly:

propHyperplaneBasic v = not (null v) ==> forAll (vectorOf (length v) arbitrary) $ \(u :: [Double]) ->
                        forAll (choose (1,99)) $ \(c :: Int) ->
                        (runNetwork (hyperplane v u ((fromIntegral c)/100)) v) == 1.0
                        && (runNetwork (hyperplane v u ((fromIntegral c)/100)) u) == (-1.0)


-- Generating random networks to prove their properties


-- prop_perceptron_equiv =   forAll (vectorOf 5 arbitrary) $ \(a :: [Double]) ->
--                           forAll (vectorOf (length a) arbitrary) $ \(b :: [Double]) ->
--                           forAll (vectorOf (length a) arbitrary) $ \(x :: [Double]) ->
--                           (runNetwork (hyperplane a b 0.5) x) == (applyPerceptron x (generatePerceptron (hyperplane a b 0.5)))

main = quickCheck propHyperplaneBasic
