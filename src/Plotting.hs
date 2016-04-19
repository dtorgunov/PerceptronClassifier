{- |
Module      : $Header$
Description : Generating plots
Copyright   : (c) Denis Torgunov, 2015-2016
License     : No license

Maintainer  : dtorgunov@leafonthewind.net
Stability   : experimental
Portability : portable (depends on Gtk2Hs)

This module is used to generate plots that help visualise the network by projecting it onto various pairs of axes.
-}
module Plotting where

import Types
import InitialSeparators
import Networks
import Parsing
import Training
import Data.Either
import Data.List

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Axis
import Graphics.Rendering.Chart.Axis.Int
import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Default.Class

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.Builder
import Control.Monad.Trans(liftIO)

-- | A list of 'Bool' values of the same size as the number of attributes in the data set.
-- Only the attributes at positions where this list has the value 'True' will be retained
-- in a projection.
type Projection = [Bool]

-- | A point in 2-dimensional space.
type Point2D = (Double, Double)

-- | A short name for functions used to determine the size and color of symbols
-- points are plotted with
type StyleFunction = Double -- ^ radius of the point
                   -> Double -- ^ thickness of the line the point is drawn with
                   -> AlphaColour Double -- ^ the colour
                   -> PointStyle

-- | Leave only the points with 'Classification' +1
plusOnes :: (Eq b, Num b) => [(a, b)] -> [(a, b)]
plusOnes = filter (\(x,c) -> c == 1)

-- | Leave only the points with 'Classification' -1
minusOnes :: (Eq b, Num b) => [(a, b)] -> [(a, b)]
minusOnes = filter (\(x, c) -> c == (-1))

-- | Default radius of plotted points
pr = 5
-- | Default line thickness for plotted points
pl = 2
-- | Default step in sample space
sampleStep = 0.1
-- | Default radius when plotting the network
nr = 3

-- | Given a 'Projection' and a list, leave only the elements for which the projection
-- is 'True'
applyProjection :: Projection -> [a] -> [a]
applyProjection [] _ = []
applyProjection _ [] = []
applyProjection (False:ps) (_:ds) = applyProjection ps ds
applyProjection (True:ps) (d:ds) = d:(applyProjection ps ds)

-- | Project all points using a given 'Projection'
project :: Projection -> [[Double]] -> [[Double]]
project p = map (applyProjection p)

-- | Apply a 'Projection' to a list of points, and convert them to 'Point2D', suitable
-- for plotting
project2D :: Projection -> [[Double]] -> [Point2D]
project2D p = map (\(x:y:_) -> (x,y)) . (project p)

-- | Generate all possible (2 dimensional) projections for a given dimensionality.
-- Makes sure each attribute is projected against each other attribute exactly once.
generateProjectors :: Int -> [Projection]
generateProjectors n = nub $ permutations ([True, True] ++ (replicate (n-2) False))

-- | Generate simple numeric lables for an n-dimentional dataset
defaultLabels :: Int -> [String]
defaultLabels n = map show [1 .. n]

-- | Determine the scale of the image to display.
-- The scale is computed by rounding the maximum and minimum
-- points, and adding/subtracting 1 (respectively) to provide some
-- space around the points
determineScale :: [Double] -> (Double, Double)
determineScale coords = (from, to)
    where
      from = (fromIntegral $ round $ minimum coords) - 1.0
      to = (fromIntegral $ round $ maximum coords) + 1.0

-- | Determine the scale of the x-axis
xScale :: [Point2D] -> (Double, Double)
xScale = determineScale . map fst 

-- | Determine the scale of the y-axis
yScale :: [Point2D] -> (Double, Double)
yScale = determineScale . map snd 

-- Axis titles. Use "default labels" for now

-- | The axis of the x-axis. Uses the default (numeric)
-- labels for attribute names.
xTitle :: Int -> [Bool] -> String
xTitle dim p = (applyProjection p (defaultLabels dim)) !! 0
-- | The axis of the y-axis. Uses the default (numeric)
-- labels for attribute names.
yTitle :: Int -> [Bool] -> String
yTitle dim p = (applyProjection p (defaultLabels dim)) !! 1

-- | Generate a plot of the data points, with a given style (i.e. crosses or plusses),
-- in a given color, and with a given 'String' used for the title when
-- generating the legend
plotPoints :: StyleFunction -> AlphaColour Double -> String -> [Point2D] -> PlotPoints Double Double
plotPoints styleFunc colour title points
    = def
      & plot_points_title .~ title
      & plot_points_values .~ points
      & plot_points_style .~ styleFunc pr pl colour

-- | A layout containg the plot of a projection of the data points, both +1 and -1, in different colours,
-- with appropriately scaled axes.
layoutPoints :: Projection -> ClassMap -> TrainingSet -> Layout Double Double
layoutPoints p classes trainingSet
    = layout_y_axis . laxis_generate .~ scaledAxis def (yScale allPoints)
      $ layout_x_axis . laxis_generate .~ scaledAxis def (xScale allPoints)
      $ layout_x_axis . laxis_title .~ (xTitle dimensions p)
      $ layout_y_axis . laxis_title .~ (yTitle dimensions p)
      $ layout_plots .~ map toPlot
        [ plotPoints plusses (opaque red) plusClass plusPoints
        , plotPoints exes (opaque blue) minusClass minusPoints
        ]

      $ def
    where
      -- the original number of dimensions
      dimensions = length $ fst $ head trainingSet
      plusPoints = project2D p $ map fst $ plusOnes trainingSet
      minusPoints = project2D p $ map fst $ minusOnes trainingSet
      allPoints = plusPoints ++ minusPoints
      plusClass = fst $ head $ plusOnes classes
      minusClass = fst $ head $ minusOnes classes

-- | Used to determine the points to plot when plotting a network.
-- Returns a list of all numbers between 2 boundaries, with a given step.
rangeOfNumbers :: (Double, Double) -> Double -> [Double]
rangeOfNumbers (from, to) step
    | from >= to = [to]
    | otherwise = from:(rangeOfNumbers (from+step, to) step)

-- | Generate sample points, from x and y scales, using the given step.
-- Constructs a set of points spanning a rectangle between the given x and y
-- coordinates.
samplePoints :: (Double, Double) -> (Double, Double) -> Double -> [Point2D]
samplePoints scaleOfX scaleOfY step = [(x, y) | x <- xs, y <- ys]
    where
      xs = rangeOfNumbers scaleOfX step
      ys = rangeOfNumbers scaleOfY step

-- | Construct a set of sample points appropriate to the given projection and
-- training set.
sampleSpace :: Projection -> TrainingSet -> [Point2D]
sampleSpace p trainingSet
    = let projectedPoints = project2D p $ map fst trainingSet
          ysc = yScale projectedPoints
          xsc = xScale projectedPoints
      in samplePoints xsc ysc sampleStep

-- | A function used internally by 'liftPoint'
liftPoint' :: Projection -> [Double] -> [Double]
liftPoint' [] _ = []
liftPoint' (_:ps) [] = 0:(liftPoint' ps [])
liftPoint' (False:ps) ds = 0:(liftPoint' ps ds)
liftPoint' (True:ps) (c:ds) = c:(liftPoint' ps ds)

-- | The opposite of projecting a point. Given a 'Projection' and a point,
-- 'lift' it to its original dimension by filling the unknown values with 0.
-- Effectively keeps the point on its plane, but makes it possible to classify
-- it using a neural network.
liftPoint :: Projection -> Point2D -> [Double]
liftPoint p (x, y) = liftPoint' p [x, y]

-- | Classify a point in the plane of a 'Projection'
classifyProjectedPoint :: Network -> Projection -> Point2D -> (Point2D, Classification)
classifyProjectedPoint net proj point = (point, cl)
    where
      cl = runNetwork net (liftPoint proj point)

-- | Generate a plot of the network points, with a given style (i.e. crosses or plusses),
-- in a given color, and with a given 'String' used for the title when
-- generating the legend. The network is plotted by plotting the classifications it assigns
-- to points on the plane it is projected onto.
plotNetwork :: AlphaColour Double -> String -> [Point2D] -> PlotPoints Double Double
plotNetwork colour title points
    = def & plot_points_title .~ title
          & plot_points_values .~ points
          & plot_points_style . point_radius .~ nr
          & plot_points_style . point_color .~ colour

-- | A layout containg the plot of a projection of the network. Plots the same points as
-- 'layoutPoints' as well as a half-transparent overlay of a selection of points in the plane
-- classified by the network.
layoutNetwork :: Projection -> ClassMap -> TrainingSet -> Network -> Layout Double Double
layoutNetwork p classes trainingSet net
    = layout_y_axis . laxis_generate .~ scaledAxis def (yScale allPoints)
      $ layout_x_axis . laxis_generate .~ scaledAxis def (xScale allPoints)
      $ layout_x_axis . laxis_title .~ (xTitle dimensions p)
      $ layout_y_axis . laxis_title .~ (yTitle dimensions p)
      $ layout_plots .~ map toPlot
        [ plotPoints plusses (opaque red) plusClass plusPoints
        , plotPoints exes (opaque blue) minusClass minusPoints
        , plotNetwork (red `withOpacity` 0.5) plusClass networkPlusPoints
        , plotNetwork (blue `withOpacity` 0.5) minusClass networkMinusPoints
        ]
      $ def
    where
      -- the original number of dimensions
      dimensions = length $ fst $ head trainingSet
      plusPoints = project2D p $ map fst $ plusOnes trainingSet
      minusPoints = project2D p $ map fst $ minusOnes trainingSet
      allPoints = plusPoints ++ minusPoints
      plusClass = fst $ head $ plusOnes classes
      minusClass = fst $ head $ minusOnes classes
      space = sampleSpace p trainingSet
      classifiedPoints = map (classifyProjectedPoint net p) space
      networkPlusPoints = map fst $ plusOnes classifiedPoints
      networkMinusPoints = map fst $ minusOnes classifiedPoints


-- | From a 'Renderable', construct a 'DrawingArea' that can be
-- used as part of a GTK+ application.
createRenderableCanvas :: Renderable a -> IO Gtk.DrawingArea
createRenderableCanvas chart = do
    canvas <- Gtk.drawingAreaNew
    Gtk.onExpose canvas $ const (updateCanvas chart canvas)
    return canvas


-- | Generate a GTK+ widget containing the point plot and the network plot
-- for a given projection, side-by-side.
generateProjection :: TrainingSet -> ClassMap -> Network -> Projection -> IO Gtk.HBox
generateProjection trainingSet classes net p = do
  points <- createRenderableCanvas (toRenderable $ layoutPoints p classes trainingSet)
  network <- createRenderableCanvas (toRenderable $ layoutNetwork p classes trainingSet net)
  box <- Gtk.hBoxNew True 0

  box `Gtk.containerAdd` points  
  box `Gtk.containerAdd` network

  return box

-- | Generate a GTK+ widget containing all possible projections (generated with 'generateProjection').
generateAllProjections :: TrainingSet -> ClassMap -> Network -> IO Gtk.VBox
generateAllProjections trainingSet classes net = do
    let projectionBox = generateProjection trainingSet classes net

    let projections = generateProjectors (length $ fst $ head trainingSet)
    projectionBoxes <- mapM projectionBox projections

    box <- Gtk.vBoxNew False 0
    Gtk.widgetSetSizeRequest box (-1) (400*(length projections)) -- replace the 400 with window size-based metric

    mapM (box `Gtk.containerAdd`) projectionBoxes

    return box
