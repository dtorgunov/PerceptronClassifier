module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Control.Monad.Trans(liftIO)

main = do

  -- Initialise the GUI
  initGUI
  builder <- builderNew
  builderAddFromFile builder "toplevel.glade"

  -- Get the named items
  -- The main window
  window <- builderGetObject builder castToWindow "mainWindow"

  -- Combo boxes
  validationMethod <- builderGetObject builder castToComboBox "validationMethod"
  algorithmVersion <- builderGetObject builder castToComboBox "algorithmVersion"
  initialSeparator <- builderGetObject builder castToComboBox "initialSeparator"

  -- Menu items
  quitMenuItem <- builderGetObject builder castToMenuItem "quitMenuItem"

  -- Add ways to exit the application
  --onDestroy window mainQuit
  window `on` deleteEvent $ liftIO mainQuit >> return False

  -- Set the combo boxes to default values
  comboBoxSetActive validationMethod 0
  comboBoxSetActive algorithmVersion 0
  comboBoxSetActive initialSeparator 0

  -- Connect signals
  quitMenuItem `on` menuItemActivated $ mainQuit

  -- Display GUI and run the main application
  widgetShowAll window
  mainGUI
