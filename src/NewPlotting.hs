module NewPoster where

import Types
import InitialSeparators
import Networks
import Parsing
import Training.Version1
import Graphics.EasyPlot
import Data.Either

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Axis
import Graphics.Rendering.Chart.Axis.Int
import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Default.Class

applyProjection :: [Bool] -> [a] -> [a]
applyProjection [] _ = []
applyProjection _ [] = []
applyProjection (False:ps) (_:ds) = applyProjection ps ds
applyProjection (True:ps) (d:ds) = d:(applyProjection ps ds)

project :: [Bool] -> [[Double]] -> [[Double]]
project p = map (applyProjection p)

-- A projection to a 2D plain
project2D :: [Bool] -> [[Double]] -> [(Double, Double)]
project2D p = map (\(x:y:_) -> (x,y)) . (project p)

-- Projector generator. Generates all possible "projectors" for a given dimentionality
generateProjectors :: Int -> [[Bool]]
generateProjectors n = nub $ permutations ([True, True] ++ (replicate (n-2) False))
                      
