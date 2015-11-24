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

plusOneMisclassed :: PlotPoints Float Float
plusOneMisclassed =
    def & plot_points_title .~ "+1, Misclassified"
        & plot_points_values .~ dataPoints
        & plot_points_style .~ (def & point_radius .~ 5 & point_color .~ (opaque red))
    where
      dataPoints = [(1,2), (3,4), (2,3)]

plusOneClassified :: PlotPoints Float Float
plusOneClassified =
    def & plot_points_title .~ "+1, Classified"
        & plot_points_values .~ dataPoints
        & plot_points_style . point_radius .~ 5
        & plot_points_style . point_color .~ opaque green
    where
      dataPoints = [(4,1), (5,5), (3,3)]
                   
minusOneMisclassed :: PlotPoints Float Float
minusOneMisclassed =
    def & plot_points_title .~ "-1, Misclassified"
        & plot_points_values .~ dataPoints
        & plot_points_style .~ plusses 5 2 (opaque red)
    where
      dataPoints = [(3,5), (6,1), (6,4)]
                   
minusOneClassified :: PlotPoints Float Float
minusOneClassified =
    def & plot_points_title .~ "-1, Classified"
        & plot_points_values .~ dataPoints
        & plot_points_style .~ plusses 5 2 (opaque green)
    where
      dataPoints = [(4,3), (1,6), (4,6)]

testLine :: PlotLines Float Float
testLine =
    def & plot_lines_title .~ "Separating Hyperplane"
       -- & plot_lines_values .~ [[(5.0,5.0), (4.0,6.0)]]
        & plot_lines_limit_values .~ [[(LMin, LMax), (LValue 5.0, LValue 5.0), (LValue 4.0, LValue 6.0), (LMax, LMin)]]
                   
layout :: Layout Float Float
layout = layout_title .~ "Neural Network"
         $ layout_y_axis . laxis_generate .~ scaledAxis def (0, 10)
         $ layout_x_axis . laxis_generate .~ scaledAxis def (0, 10)
         $ layout_plots .~ map toPlot [plusOneMisclassed, plusOneClassified, minusOneMisclassed, minusOneClassified] ++ [toPlot testLine]
         $ def

renderable :: Renderable ()
renderable = toRenderable layout

test = renderableToWindow renderable 400 400
