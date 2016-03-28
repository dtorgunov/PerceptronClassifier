module Poster where

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
import System.Environment(getArgs)

inputCoordinates :: IO TrainingSet
inputCoordinates = do
  inputs <- readCSVData "../data/iris3.data"
  return $ snd $ head $ rights [inputs]

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

-- Some projections
p1 :: [Bool]
p1 = [True, True, False, False]
p2 = [True, False, True, False]
p3 = [True, False, False, True]
p4 = [False, True, True, False]
p5 = [False, True, False, True]
p6 = [False, False, True, True]

pr = 5 -- radius of crosses/plusses
pl = 2 -- thickness of line

determineScale :: [Double] -> (Double, Double)
determineScale coords = (from, to)
    where
      from = (fromIntegral $ round $ minimum coords) - 1.0
      to = (fromIntegral $ round $ maximum coords) + 1.0

xScale :: [(Double, Double)] -> (Double, Double)
xScale ps = determineScale $ map fst ps
     
yScale :: [(Double, Double)] -> (Double, Double)
yScale ps = determineScale $ map snd ps
            
plotClassifiedPointsPlus :: [(Double, Double)] -> PlotPoints Double Double
plotClassifiedPointsPlus ps =
    def & plot_points_title .~ "Iris Veriscolor"
        & plot_points_values .~ ps
        & plot_points_style .~ plusses pr pl (opaque red)
        -- & plot_points_style . point_radius .~ pr
        -- & plot_points_style . point_color .~ opaque red
     
plotClassifiedPointsMinus :: [(Double, Double)] -> PlotPoints Double Double
plotClassifiedPointsMinus ps =
    def & plot_points_title .~ "Iris Virginica"
        & plot_points_values .~ ps
        & plot_points_style .~ exes pr pl (opaque blue)
        -- & plot_points_style . point_radius .~ pr
        -- & plot_points_style . point_color .~ opaque blue

layoutOriginalPoints :: [Bool] -> TrainingSet -> Layout Double Double
layoutOriginalPoints p ts = layout_title .~ "Iris Classes"
                            $ layout_y_axis . laxis_generate .~ scaledAxis def ysc
                            $ layout_x_axis . laxis_generate .~ scaledAxis def xsc
                            $ layout_plots .~ map toPlot [ plotClassifiedPointsPlus plusPoints
                                                         , plotClassifiedPointsMinus minusPoints
                                                         ]
                            $ def
    where
      plusPoints = project2D p $ map fst $ filter (\(_,c) -> c == 1) ts
      minusPoints = project2D p $ map fst $ filter (\(_,c) -> c == -1) ts
      ysc = yScale (minusPoints ++ plusPoints)
      xsc = xScale (minusPoints ++ plusPoints)

renderableOriginal :: [Bool] -> TrainingSet -> Renderable ()
renderableOriginal p ts = toRenderable $ layoutOriginalPoints p ts

-- plotClassifiedPoints :: [Bool] -> TrainingSet -> IO Bool
-- plotClassifiedPoints p points = do
--   let plusOnes = map fst $ filter (\(_,c) -> c == 1) points
--   let minusOnes = map fst $ filter (\(_,c) -> c == -1) points
--   let plusOnes' = project2D p plusOnes
--   let minusOnes' = project2D p minusOnes
--   plot X11 $ [ Data2D [Color Red, Title "Iris Versicolor"] [] plusOnes', Data2D [Color Blue, Title "Iris Virginica"] [] minusOnes']

-- plotDataAsProjection :: [Bool] -> IO ()
-- plotDataAsProjection p = do
--   i <- inputCoordinates
--   plotClassifiedPoints p i
--   return ()

plotDataAsProjection :: [Bool] -> IO ()
plotDataAsProjection p = do
   i <- inputCoordinates
   renderableToWindow (renderableOriginal p i) 400 400
   
  
  -- try with p6 for now
