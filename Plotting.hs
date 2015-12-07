module Main where

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

plusOnes = [(4,1), (5,3), (7,2)]
minusOnes = [(1,8), (4,5), (6,7), (12, 6)]

plusOneMisclassed :: PlotPoints Float Float
plusOneMisclassed =
    def & plot_points_title .~ "+1, Misclassified"
        & plot_points_values .~ dataPoints
        & plot_points_style .~ (def & point_radius .~ 5 & point_color .~ (opaque red))
    where
      dataPoints = plusOnes

plusOneClassified :: PlotPoints Float Float
plusOneClassified =
    def & plot_points_title .~ "+1, Classified"
        & plot_points_values .~ dataPoints
        & plot_points_style . point_radius .~ 5
        & plot_points_style . point_color .~ opaque green
    where
      dataPoints = []
                   
minusOneMisclassed :: PlotPoints Float Float
minusOneMisclassed =
    def & plot_points_title .~ "-1, Misclassified"
        & plot_points_values .~ dataPoints
        & plot_points_style .~ plusses 5 2 (opaque red)
    where
      dataPoints = minusOnes
                   
minusOneClassified :: PlotPoints Float Float
minusOneClassified =
    def & plot_points_title .~ "-1, Classified"
        & plot_points_values .~ dataPoints
        & plot_points_style .~ plusses 5 2 (opaque green)
    where
      dataPoints = []

testLine :: PlotLines Float Float
testLine =
    def & plot_lines_title .~ "Separating Hyperplane"
       -- & plot_lines_values .~ [[(5.0,5.0), (4.0,6.0)]]
        & plot_lines_limit_values .~ [[(LMin, LMax), (LValue 5.0, LValue 5.0), (LValue 4.0, LValue 6.0), (LMax, LMin)]]

layout1 :: Layout Float Float
layout1 = layout_title .~ "Neural Network"
         $ layout_y_axis . laxis_generate .~ scaledAxis def (0, 15)
         $ layout_x_axis . laxis_generate .~ scaledAxis def (0, 15)
         $ layout_plots .~ map toPlot [plusOneMisclassed, plusOneClassified, minusOneMisclassed, minusOneClassified]
         $ def

plusOneMisclassed1 :: PlotPoints Float Float
plusOneMisclassed1 =
    def & plot_points_title .~ "+1, Misclassified"
        & plot_points_values .~ dataPoints
        & plot_points_style .~ (def & point_radius .~ 5 & point_color .~ (opaque red))
    where
      dataPoints = []

plusOneClassified1 :: PlotPoints Float Float
plusOneClassified1 =
    def & plot_points_title .~ "+1, Classified"
        & plot_points_values .~ dataPoints
        & plot_points_style . point_radius .~ 5
        & plot_points_style . point_color .~ opaque green
    where
      dataPoints = plusOnes
                   
minusOneMisclassed1 :: PlotPoints Float Float
minusOneMisclassed1 =
    def & plot_points_title .~ "-1, Misclassified"
        & plot_points_values .~ dataPoints
        & plot_points_style .~ plusses 5 2 (opaque red)
    where
      dataPoints = [(12,6)]
                   
minusOneClassified1 :: PlotPoints Float Float
minusOneClassified1 =
    def & plot_points_title .~ "-1, Classified"
        & plot_points_values .~ dataPoints
        & plot_points_style .~ plusses 5 2 (opaque green)
    where
      dataPoints = [(1,8), (4,5), (6,7)]

line1 :: PlotLines Float Float
line1 =
    def & plot_lines_title .~ "Separating Hyperplane"
        & plot_lines_values .~ [[(0.0,1.75), (15, 9.25)]]
                   
layout2 :: Layout Float Float
layout2 = layout_title .~ "Neural Network"
         $ layout_y_axis . laxis_generate .~ scaledAxis def (0, 15)
         $ layout_x_axis . laxis_generate .~ scaledAxis def (0, 15)
         $ layout_plots .~ map toPlot [plusOneMisclassed1, plusOneClassified1, minusOneMisclassed1, minusOneClassified1] ++ [toPlot line1]
         $ def

minusOneMisclassed2 :: PlotPoints Float Float
minusOneMisclassed2 =
    def & plot_points_title .~ "-1, Misclassified"
        & plot_points_values .~ dataPoints
        & plot_points_style .~ plusses 5 2 (opaque red)
    where
      dataPoints = []
                   
minusOneClassified2 :: PlotPoints Float Float
minusOneClassified2 =
    def & plot_points_title .~ "-1, Classified"
        & plot_points_values .~ dataPoints
        & plot_points_style .~ plusses 5 2 (opaque green)
    where
      dataPoints = minusOnes
line2 :: PlotLines Float Float
line2 =
    def & plot_lines_title .~ "Separating Hyperplane"
        & plot_lines_values .~ [[(10.41, 0), (3.99, 15)]]
         
layout3 :: Layout Float Float
layout3 = layout_title .~ "Neural Network"
         $ layout_y_axis . laxis_generate .~ scaledAxis def (0, 15)
         $ layout_x_axis . laxis_generate .~ scaledAxis def (0, 15)
         $ layout_plots .~ map toPlot [plusOneMisclassed1, plusOneClassified1, minusOneMisclassed2, minusOneClassified2] ++ [toPlot line2, toPlot line1]
         $ def

renderable1 :: Renderable ()
renderable1 = toRenderable layout1

renderable2 :: Renderable ()
renderable2 = toRenderable layout2

renderable3 :: Renderable ()
renderable3 = toRenderable layout3
              
test1 = renderableToWindow renderable1 400 400
test2 = renderableToWindow renderable2 400 400
test3 = renderableToWindow renderable3 400 400

main = do
  args <- getArgs
  let a1 = head args
  case a1 of
    "0" -> test1
    "1" -> test2
    "2" -> test3
  
