module Main where

import Training
import Networks
import Types
import System.Random
import Data.List
import System.Environment
import Parsing

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
-- Given a list, split it roughly p/(100 - p)
splitList' :: (RandomGen g, Eq a) => Int -> g -> [a] -> ([a], [a])
splitList' p g xs = (training, verification)
    where
      indexes = takeUnique ((length xs - 1) * p `div` 100) (randomRs (0, (length xs - 1)) g) []
      training = takeIndexes indexes xs []
      verification = xs \\ training

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn ("Usage: " ++ progname ++ " CSV-input-file")

testOn :: String -> Int -> IO ()
testOn filename p = do
  dt <- readCSVData filename
  case dt of
    Left err -> putStrLn err
    Right (classMap, dataSet) -> do
                    let class1 = classMap !! 0
                    let class2 = classMap !! 1
                    putStrLn ("Assigning classes: " ++ (show class1) ++ ", " ++ (show class2))
                    gen <- getStdGen
                    let splitList = splitList' p
                    let (training, verification) = splitList gen dataSet
                    let network = createNetwork training
                    let results = map (\(x, y) -> x == y) $ map (\(x,y) -> ((runNetwork network) x, y)) verification
                    let errors = length $ filter (==False) results
                    putStrLn ("After training on " ++ (show p) ++ "% of the data set, there were " ++ (show errors) ++ " errors during verification.")
                    putStrLn "Verified on the following values: "
                    mapM_ putStrLn $ map show verification
                    putStrLn "The resulting network is as follows:"
                    putStrLn $ (show . net) network
                             
  
main :: IO ()
main = do
  args <- getArgs
  if (length args) /= 1 then usage else testOn (args !! 0) 70
  
