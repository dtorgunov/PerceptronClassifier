module Main where

import Graphics.UI.Gtk

import Gui

import Paths_NeuralNetworks

main :: IO ()
main = do
  guiBuilder <- getDataFileName "toplevel.glade"
  gui <- prepareGui guiBuilder
  -- Display GUI and run the main application
  widgetShowAll $ rootWindow gui
  (consoleOut gui) "Initialisation finished."
  mainGUI
