module Parsing where

import Text.ParserCombinators.Parsec
import Data.List
import Data.Maybe
import Types

data TrainingData = TrainingData { inputs :: [Double]
                                 , cl :: Class
                                 }
                    deriving Show
type Class = String
type ClassMap = [(String, Double)]

csvFile = do
  result <- many line
  eof
  return result

-- Based on real world haskell
line :: GenParser Char st [String]
line = do result <- cells
          eol
          return result

cells = do first <- cellContent
           rest <- remainingCells
           return (first : rest)

remainingCells = (char ',' >> cells) <|> return ([])

cellContent = many (noneOf ",\n")

eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

-- Read coordinates in as Doubles and separate the classification at
-- the end
prepareData' :: [[String]] -> [TrainingData]
prepareData' = filter (not . empty) . map toDataPoint
    where
      empty :: TrainingData -> Bool
      empty td = length (inputs td) == 0
      toDataPoint :: [String] -> TrainingData
      toDataPoint ds = TrainingData inputs cl
          where
            cl = head $ reverse $ ds
            inputs = map read (reverse $ drop 1 $ reverse ds)

-- Make a list of unique classes that are present in the input data
uniqueClasses :: [TrainingData] -> [String]
uniqueClasses = nub . map cl

-- Convert the classes to +1/-1
numericClasses :: ClassMap -> TrainingData -> TrainingInput
numericClasses classMap dt = (inputs dt, fromJust $ lookup (cl dt) classMap)

-- Prepare data for use. Convert classes to numbers, and report errors
-- if more than 2 distinct classes are present
prepareData :: [[String]] -> Either String (ClassMap, TrainingSet)
prepareData parsed = let dt = prepareData' parsed
                         classes = uniqueClasses dt
                     in if (length classes) /= 2 then
                            Left "Need 2 unique classes to work with"
                        else
                            Right $ ((zip classes [1.0,(-1.0)]),
                                     map (numericClasses
                                          (zip classes [1.0,(-1.0)]))
                                     dt)
                                     
-- Read a file in and prepare data for use
readCSVData :: String -> IO (Either String (ClassMap, TrainingSet))
readCSVData path = do
  contents <- readFile path
  case (parseCSV contents) of
    Left err -> return $ Left (show err)
    Right dt -> return $ prepareData dt
