module Main where

import Graphics.UI.Gtk

import Gui

main :: IO ()
main = do
  gui <- prepareGui
  -- Display GUI and run the main application
  widgetShowAll $ rootWindow gui
  (consoleOut gui) "Initialisation finished."
  mainGUI
