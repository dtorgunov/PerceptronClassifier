module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

main = do
  initGUI
--  Just xml <- xmlNew "basicWindw.glade"
--  window <- xmlGetWidget xml castToWindow "applicationWindow1"
  builder <- builderNew
  builderAddFromFile builder "toplevel.glade"
  window <- builderGetObject builder castToWindow "mainWindow"
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
