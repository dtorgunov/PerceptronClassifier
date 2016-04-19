{- |
Module      : $Header$
Description : Parsing data in for classification
Copyright   : (c) Denis Torgunov, 2015-2016
License     : No license

Maintainer  : dtorgunov@leafonthewind.net
Stability   : experimental
Portability : portable (depends on Gtk2Hs)

This module holds all the functions necessary to read training sets in from files.

Based on Real World Haskell.
-}
module Parsing (
                readCSVData
               )where

import Text.ParserCombinators.Parsec
import Data.List
import Data.Maybe
import Types

-- | Abstracts the data as a list of doubles and a class, which is later translated to +1 or -1.
data TrainingData
    = TrainingData { inputs :: [Double] -- ^ The input vector
                   , cl :: Class -- ^ The class this point should be classified as
                   }
                    deriving Show

-- | A class can be any 'String'
type Class = String

-- | Represnts a CSV file, as a collection of many lines, followed by an end of file
csvFile = do
  result <- many line
  eof
  return result

-- | A line is a collection of cells, followed by a newline
line :: GenParser Char st [String]
line = do result <- cells
          eol
          return result

-- | A cell is a list of "cell contents"
cells = do first <- cellContent
           rest <- remainingCells
           return (first : rest)

-- | Makes sure we keep reading cells as long as there are commas, and return [] after the last cell
remainingCells = (char ',' >> cells) <|> return ([])

-- | The content of a cell is anything other than a comma or newline
cellContent = many (noneOf ",\n")

-- | A simple newline
eol = char '\n'

-- | Parse the input using 'csvFile' above
parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

-- | Read coordinates in as 'Double's and separate the classification at the end
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

-- | Make a list of unique classes that are present in the input data, in order to construct
-- the 'ClassMap'
uniqueClasses :: [TrainingData] -> [String]
uniqueClasses = nub . map cl

-- | Convert the classes to +1.0/-1.0
numericClasses :: ClassMap -> TrainingData -> TrainingInput
numericClasses classMap dt = (inputs dt, fromJust $ lookup (cl dt) classMap)

-- | Prepare data for use. Convert classes to numbers, and report errors
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
                                     
-- | Read a file in and prepare data for use
readCSVData :: String -> IO (Either String (ClassMap, TrainingSet))
readCSVData path = do
  contents <- readFile path
  case (parseCSV contents) of
    Left err -> return $ Left (show err)
    Right dt -> return $ prepareData dt
