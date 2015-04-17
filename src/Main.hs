module Main where

import Training
import Inputs
import System.Random
import Data.List

-- Take n unique elements from a given list
takeUnique :: (Eq a) => Int -> [a] -> [a] -> [a]
takeUnique 0 _ acc = acc
takeUnique n (i:ins) acc | i `elem` acc = takeUnique n ins acc
                         | otherwise = takeUnique (n-1) ins (i:acc)

-- Take the elements at indexes in is from the list of xs
takeIndexes :: [Int] -> [a] -> [a] -> [a]
takeIndexes [] _ acc = acc
takeIndexes (i:is) xs acc = takeIndexes is xs ((xs !! i):acc)
                                       

-- A function to fascilitate verification.
-- Given a list, split it roughly 70/30
splitList :: (RandomGen g, Eq a) => g -> [a] -> ([a], [a])
splitList g xs = (training, verification)
    where
      indexes = takeUnique ((length xs - 1) * 70 `div` 100) (randomRs (0, (length xs - 1)) g) []
      training = takeIndexes indexes xs []
      verification = xs \\ training

main :: IO ()
main = do
  gen <- getStdGen
  let (training, verification) = splitList gen iris
  let net = createNetwork training
  let results = map (\(x, y) -> x == y) $ map (\(x,y) -> (net x, y)) verification
  let errors = length $ filter (==False) results
  putStrLn ("After training on 70% of the data set, there were " ++ (show errors) ++ " errors during verification.")
  putStrLn "Verified on the following values: "
  mapM_ putStrLn $ map show verification
  
