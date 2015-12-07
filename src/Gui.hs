module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

main = do

  -- Initialise the GUI
  initGUI
  builder <- builderNew
  builderAddFromFile builder "toplevel.glade"

  -- Get the named items
  window <- builderGetObject builder castToWindow "mainWindow"
  validationMethod <- builderGetObject builder castToComboBox "validationMethod"
  algorithmVersion <- builderGetObject builder castToComboBox "algorithmVersion"
  initialSeparator <- builderGetObject builder castToComboBox "initialSeparator"

  -- Add ways to exit the application
  onDestroy window mainQuit

  -- Set the combo boxes to default values
  comboBoxSetActive validationMethod 0
  comboBoxSetActive algorithmVersion 0
  comboBoxSetActive initialSeparator 0

  -- Display GUI and run the main application
  widgetShowAll window
  mainGUI
