{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.QuickCheck
import Test.QuickCheck.Property as P
import Types
import Networks

prop_perceptron_equiv =   forAll (vectorOf 5 arbitrary) $ \(a :: [Double]) ->
                          forAll (vectorOf (length a) arbitrary) $ \(b :: [Double]) ->
                          forAll (vectorOf (length a) arbitrary) $ \(x :: [Double]) ->
                          (runNetwork (hyperplane a b 0.5) x) == (applyPerceptron x (generatePerceptron (hyperplane a b 0.5)))
    -- where
    --   classifierEquivalence x = (runNetwork (hyperplane a b 0.5) x) == (applyPerceptron x (generatePerceptron (hyperplane a b 0.5)))

main = quickCheck prop_perceptron_equiv
