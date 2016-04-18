{-# LANGUAGE ScopedTypeVariables #-}
import Test.QuickCheck
import Test.QuickCheck.Property as P
import Types
import Networks
import Training.Version10
import InitialSeparators

-- Generating a random data set
-- The datasets are "sized" based on dimentionaity

twoPoints :: Int -> Gen TrainingSet
twoPoints n = do
  x <- vectorOf n arbitrary
  y <- (vectorOf n arbitrary) `suchThat` (/= x)
  return [(x,1.0), (y,(-1.0))]

-- Generates m n-dimentional points. If m < 2 => m is set to 2
mPoints :: Int -> Int -> Gen TrainingSet
mPoints m n
    | m < 2 = mPoints 2 n
    | otherwise = mPoints' m n []

mPoints' :: Int -> Int -> TrainingSet -> Gen TrainingSet
mPoints' m n accum
    | m == 0 = return $ accum
    | null accum = do -- ensures there is at least one +1 and at least one -1 point
  initial <- twoPoints n
  mPoints' (m-2) n initial
    | otherwise = do
    point <- (vectorOf n arbitrary) `suchThat` (\p -> not $ p `elem` (map fst accum))
    classification <- oneof [return 1.0, return (-1.0)]
    mPoints' (m-1) n ((point,classification):accum)


-- Assumes no initial separators
prop_PerfectTraining :: TrainingSet -> Bool
prop_PerfectTraining ts = and $ map confirmClass ts
    where
      network = createNetwork noSeparator ts
      confirmClass (i, c) = (runNetwork network i) == c

prop_PerceptronEquivalence :: Property
prop_PerceptronEquivalence = forAll (vectorOf 5 arbitrary) $ \(p :: [Double]) ->
                             (forAll $ mPoints 50 5) $ \ts ->
                             (\net ->
                              (runNetwork net p) == (Networks.classify p (perceptronNetwork net))) (createNetwork noSeparator ts)

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
       >> quickCheck ((forAll $ mPoints 50 5) prop_PerfectTraining)
       >> quickCheck prop_PerceptronEquivalence
       
