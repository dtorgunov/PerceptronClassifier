module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
    
import Control.Monad.Trans(liftIO)
import Data.IORef

import Training
import Parsing
import Types
import Networks

-- Train on 100% of data for this prototype
trainNetwork :: FilePath -> IO (TrainingSet, Network)
trainNetwork filename = do
  dt <- readCSVData filename
  case dt of
    Left err -> putStrLn err >> return ([],(makeNetwork Empty))
    Right (classMap, dataSet) -> do
                          -- Use 100% as training for this example
                          let training = dataSet
                          let network = createNetwork training
                          return (dataSet, network)

-- Set the labels based on results
displayConfusionMatrix :: (LabelClass l) => l -> l -> l -> l -> l -> [(Double, Double)] -> IO ()
displayConfusionMatrix truePositives falseNegatives falsePositives trueNegatives accuracy results
    = do
  let tp = length $ filter (\(x,y) -> (x == 1) && (y == 1)) results
  let tn = length $ filter (\(x,y) -> (x == (-1)) && (y == (-1))) results
  let fp = length $ filter (\(returned, expected) -> ((returned == 1)
                                                     && (expected == (-1)))) results
  let fn = length $ filter (\(x,y) -> (x== (-1)) && (y == 1)) results
--  let acc = (tp+tn)*100.0 / (1.0*(length results))
  labelSetText truePositives (show tp)
  labelSetText falseNegatives (show fn)
  labelSetText falsePositives (show fp)
  labelSetText trueNegatives (show tn)
  labelSetText accuracy "TBA"

-- evaluateNetwork :: IO ()
evaluateNetwork datafile fillInMatrix = do
  filename <- readIORef datafile
  (ds, network) <- trainNetwork filename

  let results = map (\(x,y) -> ((runNetwork network) x, y)) ds
                            
  fillInMatrix results

chooseDataset datafile window = do
  fcd <- fileChooserDialogNew (Just "Choose a data set") (Just window) FileChooserActionOpen
         [("Cancel", ResponseCancel)
         ,("Open", ResponseAccept)]
  widgetShow fcd
  result <- dialogRun fcd
  case result of
    ResponseCancel -> return ()
    ResponseAccept -> do
                filename <- fileChooserGetFilename fcd
                case filename of
                  Nothing -> return ()
                  Just f -> writeIORef datafile f

  widgetDestroy fcd
  return ()
  

main = do

  -- Create a variable to store the path to the data set later on
  datafile <- newIORef "../data/iris1.data"

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

  -- Buttons
  regenerate <- builderGetObject builder castToButton "regenerate"

  -- Confusion matrix labels
  truePositives <- builderGetObject builder castToLabel "truePositives"
  falseNegatives <- builderGetObject builder castToLabel "falseNegatives"
  falsePositives <- builderGetObject builder castToLabel "falsePositives"
  trueNegatives <- builderGetObject builder castToLabel "trueNegatives"
  accuracy <- builderGetObject builder castToLabel "accuracy"

  -- Menu items
  loadDataMenuItem <- builderGetObject builder castToMenuItem "loadDataMenuItem"
  saveNetworkMenuItem <- builderGetObject builder castToMenuItem "saveNetworkMenuItem"
  quitMenuItem <- builderGetObject builder castToMenuItem "quitMenuItem"
  helpMenu <- builderGetObject builder castToMenuItem "helpMenu"

  -- Add ways to exit the application
  window `on` deleteEvent $ liftIO mainQuit >> return False

  -- Set the combo boxes to default values
  comboBoxSetActive validationMethod 0
  comboBoxSetActive algorithmVersion 0
  comboBoxSetActive initialSeparator 0

  -- Create the function for confusion labels
  let fillInMatrix = displayConfusionMatrix truePositives falseNegatives falsePositives trueNegatives accuracy

  -- Connect signals
  quitMenuItem `on` menuItemActivated $ mainQuit
  regenerate `on` buttonActivated $ (evaluateNetwork datafile fillInMatrix)
  loadDataMenuItem `on` menuItemActivated $ (chooseDataset datafile window) >> (buttonClicked regenerate)

  -- Disable the elements not currently used
  widgetSetSensitive validationMethod False
  widgetSetSensitive algorithmVersion False
  widgetSetSensitive initialSeparator False
  widgetSetSensitive saveNetworkMenuItem False
  widgetSetSensitive helpMenu False
  
  -- Display GUI and run the main application
  widgetShowAll window
  mainGUI
