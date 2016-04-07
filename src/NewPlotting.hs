module NewPoster where

import Types
import InitialSeparators
import Networks
import Parsing
import Training.Version1
import Data.Either
import Data.List

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Axis
import Graphics.Rendering.Chart.Axis.Int
import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Default.Class

type Projection = [Bool]
type Point2D = (Double, Double)
type StyleFunction = Double -> Double -> AlphaColour Double -> PointStyle
type ClassMap = [(String, Double)] -- to be consistent with Parsing. Might need moving to Types

-- Also used by training algorithms. Move to Types, maybe?
plusOnes = filter (\(x,c) -> c == 1)
minusOnes = filter (\(x, c) -> c == (-1))

-- Some "constants" that should be tweaked later
pr = 5 -- plus/cross radius
pl = 2 -- line thickness (?)
sampleStep = 0.1
nr = 3 -- radius for network points

applyProjection :: Projection -> [a] -> [a]
applyProjection [] _ = []
applyProjection _ [] = []
applyProjection (False:ps) (_:ds) = applyProjection ps ds
applyProjection (True:ps) (d:ds) = d:(applyProjection ps ds)

project :: Projection -> [[Double]] -> [[Double]]
project p = map (applyProjection p)

-- A projection to a 2D plain
project2D :: Projection -> [[Double]] -> [Point2D]
project2D p = map (\(x:y:_) -> (x,y)) . (project p)

-- Projector generator. Generates all possible "projectors" for a given dimentionality
generateProjectors :: Int -> [Projection]
generateProjectors n = nub $ permutations ([True, True] ++ (replicate (n-2) False))

-- Generate simple numeric lables for an n-dimentional dataset
defaultLabels :: Int -> [String]
defaultLabels n = map show [1 .. n]

-- Determine the scale of the image to display
-- make it the max/min points, rounded, +/-1
determineScale :: [Double] -> (Double, Double)
determineScale coords = (from, to)
    where
      from = (fromIntegral $ round $ minimum coords) - 1.0
      to = (fromIntegral $ round $ maximum coords) + 1.0

xScale :: [Point2D] -> (Double, Double)
xScale = determineScale . map fst 
yScale :: [Point2D] -> (Double, Double)
yScale = determineScale . map snd 

-- Axis titles. Use "default labels" for now
xTitle :: Int -> [Bool] -> String
xTitle dim p = (applyProjection p (defaultLabels dim)) !! 0
yTitle :: Int -> [Bool] -> String
yTitle dim p = (applyProjection p (defaultLabels dim)) !! 1

plotPoints :: StyleFunction -> AlphaColour Double -> String -> [Point2D] -> PlotPoints Double Double
plotPoints styleFunc colour title points
    = def
      & plot_points_title .~ title
      & plot_points_values .~ points
      & plot_points_style .~ styleFunc pr pl colour

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

-- Used determine the points to plot when plotting a network
rangeOfNumbers :: (Double, Double) -> Double -> [Double]
rangeOfNumbers (from, to) step
    | from >= to = [to]
    | otherwise = from:(rangeOfNumbers (from+step, to) step)

samplePoints :: (Double, Double) -> (Double, Double) -> Double -> [Point2D]
samplePoints scaleOfX scaleOfY step = [(x, y) | x <- xs, y <- ys]
    where
      xs = rangeOfNumbers scaleOfX step
      ys = rangeOfNumbers scaleOfY step

sampleSpace :: Projection -> TrainingSet -> [Point2D]
sampleSpace p trainingSet
    = let projectedPoints = project2D p $ map fst trainingSet
          ysc = yScale projectedPoints
          xsc = xScale projectedPoints
      in samplePoints xsc ysc sampleStep

liftPoint' :: Projection -> [Double] -> [Double]
liftPoint' [] _ = []
liftPoint' (_:ps) [] = 0:(liftPoint' ps [])
liftPoint' (False:ps) ds = 0:(liftPoint' ps ds)
liftPoint' (True:ps) (c:ds) = c:(liftPoint' ps ds)

liftPoint :: Projection -> Point2D -> [Double]
liftPoint p (x, y) = liftPoint' p [x, y]

classifyProjectedPoint :: Network -> Projection -> Point2D -> (Point2D, Classification)
classifyProjectedPoint net proj point = (point, cl)
    where
      cl = runNetwork net (liftPoint proj point)

plotNetwork :: AlphaColour Double -> String -> [Point2D] -> PlotPoints Double Double
plotNetwork colour title points
    = def & plot_points_title .~ title
          & plot_points_values .~ points
          & plot_points_style . point_radius .~ nr
          & plot_points_style . point_color .~ colour

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

main = do
  inputs <- readCSVData "../data/iris3.data"
  let trainingSet = snd $ head $ rights [inputs]
  let sep = noSeparator
  let alg = createNetwork

  net <- case inputs of
    Left err -> return emptyNet
    Right (_, ds) -> return $ (alg sep) ds

  -- renderableToWindow (toRenderable $ layoutPoints [False, False, True, True] (fst $ head $ rights [inputs]) trainingSet) 400 400
  renderableToWindow (toRenderable $ layoutNetwork [False, False, True, True] (fst $ head $ rights [inputs]) trainingSet net) 400 400
