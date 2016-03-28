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

labels = ["Sepal Length", "Sepal Width", "Petal Length", "Petal Width"]

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
     
plotClassifiedPointsMinus :: [(Double, Double)] -> PlotPoints Double Double
plotClassifiedPointsMinus ps =
    def & plot_points_title .~ "Iris Virginica"
        & plot_points_values .~ ps
        & plot_points_style .~ exes pr pl (opaque blue)

xTitle :: [Bool] -> String
xTitle p = (applyProjection p labels) !! 0

yTitle :: [Bool] -> String
yTitle p = (applyProjection p labels) !! 1

layoutOriginalPoints :: [Bool] -> TrainingSet -> Layout Double Double
layoutOriginalPoints p ts = layout_title .~ "Iris Data Projection"
                            $ layout_y_axis . laxis_generate .~ scaledAxis def ysc
                            $ layout_x_axis . laxis_generate .~ scaledAxis def xsc
                            $ layout_x_axis . laxis_title .~ (xTitle p) 
                            $ layout_y_axis . laxis_title .~ (yTitle p) 
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
                          
plotDataAsProjection :: [Bool] -> IO ()
plotDataAsProjection p = do
   i <- inputCoordinates
   renderableToWindow (renderableOriginal p i) 400 400
   
trainedNetwork :: IO Network
trainedNetwork = do
  i <- readCSVData "../data/iris3.data"
  let sep = noSeparator
  let alg = createNetwork

  case i of
    Left err -> return emptyNet
    Right (_, ds) -> return $ (alg sep) ds

rangeOfNumbers :: (Double, Double) -> Double -> [Double]
rangeOfNumbers (from, to) step
    | from >= to = [to]
    | otherwise = from:(rangeOfNumbers (from+step, to) step)

samplePoints :: (Double, Double) -> (Double, Double) -> Double -> [(Double, Double)]
samplePoints scaleOfX scaleOfY step = [(x, y) | x <- xs, y <- ys]
    where
      xs = rangeOfNumbers scaleOfX step
      ys = rangeOfNumbers scaleOfY step

liftPoint' :: [Bool] -> [Double] -> [Double]
liftPoint' [] _ = []
liftPoint' (_:ps) [] = 0:(liftPoint' ps [])
liftPoint' (False:ps) ds = 0:(liftPoint' ps ds)
liftPoint' (True:ps) (c:ds) = c:(liftPoint' ps ds)

liftPoint :: [Bool] -> (Double, Double) -> [Double]
liftPoint p (x, y) = liftPoint' p [x, y]
                           

classifyProjectedPoint :: Network -> [Bool] -> (Double, Double) -> ((Double, Double), Classification)
classifyProjectedPoint net proj point = (point, cl)
    where
      cl = runNetwork net (liftPoint proj point)

sampleStep = 0.1
nr = 3 -- radius for network points

sampleSpace :: [Bool] -> IO [(Double, Double)]
sampleSpace p = do
  i <- inputCoordinates
  let projectedPoints = project2D p $ map fst i
  let ysc = yScale projectedPoints
  let xsc = xScale projectedPoints
  return $ samplePoints xsc ysc sampleStep

plotNetworkPlus :: [(Double, Double)] -> PlotPoints Double Double
plotNetworkPlus ps =
    def & plot_points_title .~ "Iris Veriscolor"
        & plot_points_values .~ ps
        & plot_points_style . point_radius .~ nr
        & plot_points_style . point_color .~ opaque red
     
plotNetworkMinus :: [(Double, Double)] -> PlotPoints Double Double
plotNetworkMinus ps =
    def & plot_points_title .~ "Iris Virginica"
        & plot_points_values .~ ps
        & plot_points_style . point_radius .~ nr
        & plot_points_style . point_color .~ opaque blue

layoutNetwork :: [Bool] -> [((Double, Double), Classification)] -> Layout Double Double
layoutNetwork p ps = layout_title .~ "Network Projection"
                   $ layout_x_axis . laxis_title .~ (xTitle p) 
                   $ layout_y_axis . laxis_title .~ (yTitle p) 
                   $ layout_plots .~ map toPlot [ plotNetworkPlus plusPoints
                                                , plotNetworkMinus minusPoints
                                                ]
                   $ def
    where
      plusPoints = map fst $ filter (\(_,c) -> c == 1) ps
      minusPoints = map fst $ filter (\(_,c) -> c == -1) ps
                   

plotNetworkAsProjection :: [Bool] -> IO ()
plotNetworkAsProjection p = do
  n <- trainedNetwork
  space <- sampleSpace p
  let classifiedPoints = map (classifyProjectedPoint n p) space
  renderableToWindow (toRenderable $ layoutNetwork p classifiedPoints) 400 400
  

plotNetworkPlus' :: [(Double, Double)] -> PlotPoints Double Double
plotNetworkPlus' ps =
    def & plot_points_title .~ "Iris Veriscolor"
        & plot_points_values .~ ps
        & plot_points_style . point_radius .~ nr
        & plot_points_style . point_color .~ red `withOpacity` 0.5
     
plotNetworkMinus' :: [(Double, Double)] -> PlotPoints Double Double
plotNetworkMinus' ps =
    def & plot_points_title .~ "Iris Virginica"
        & plot_points_values .~ ps
        & plot_points_style . point_radius .~ nr
        & plot_points_style . point_color .~ blue `withOpacity` 0.5

         
layoutJoint :: [Bool] -> TrainingSet -> [((Double, Double), Classification)]-> Layout Double Double
layoutJoint p ts ps = layout_title .~ "Combined Projection"
                      $ layout_y_axis . laxis_generate .~ scaledAxis def ysc
                      $ layout_x_axis . laxis_generate .~ scaledAxis def xsc
                      $ layout_x_axis . laxis_title .~ (xTitle p) 
                      $ layout_y_axis . laxis_title .~ (yTitle p) 
                      $ layout_plots .~ map toPlot [ plotClassifiedPointsPlus plusPoints
                                                   , plotClassifiedPointsMinus minusPoints
                                                   , plotNetworkPlus' np
                                                   , plotNetworkMinus' nm
                                                   ]
                      $ def
    where
      plusPoints = project2D p $ map fst $ filter (\(_,c) -> c == 1) ts
      minusPoints = project2D p $ map fst $ filter (\(_,c) -> c == -1) ts
      ysc = yScale (minusPoints ++ plusPoints)
      xsc = xScale (minusPoints ++ plusPoints)
      np = map fst $ filter (\(_,c) -> c == 1) ps
      nm = map fst $ filter (\(_,c) -> c == -1) ps

plotJoint :: [Bool] -> IO ()
plotJoint p = do
  i <- inputCoordinates
  n <- trainedNetwork
  space <- sampleSpace p
  let classifiedPoints = map (classifyProjectedPoint n p) space
  renderableToWindow (toRenderable $ layoutJoint p i classifiedPoints) 400 400
