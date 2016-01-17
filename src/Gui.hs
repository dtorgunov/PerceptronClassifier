module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
    
import Control.Monad.Trans(liftIO)
import Data.IORef
import System.Random
import Data.List

import Training
import qualified TrainingOldAugmented as TrainingOld
import Parsing
import Types
import Networks
import InitialSeparators
import Validation

trainNetwork :: FilePath -> (TrainingSet -> Network) -> (String -> IO ()) -> IO (ConfusionMatrixData, ConfusionMatrixData, Network)
trainNetwork filename trainingMethod putOut = do
  dt <- readCSVData filename
  case dt of
    Left err -> putStrLn err >> return (NoData, NoData, (makeNetwork Empty))
    Right (classMap, dataSet) -> do
                          let class1 = classMap !! 0
                          let class2 = classMap !! 1
                          putOut ("Assigning classes: " ++ (show class1) ++ ", " ++ (show class2))
                          gen <- getStdGen
                          -- 70/30 training
                          return $ splitValidation 70 gen dataSet trainingMethod

-- Set the labels based on results
displayConfusionMatrix :: (LabelClass l) => l -> l -> l -> l -> l -> ConfusionMatrixData -> IO ()
displayConfusionMatrix truePositives falseNegatives falsePositives trueNegatives accuracy matData
    = do
  labelSetText truePositives (show $ tp matData)
  labelSetText falseNegatives (show $ fn matData)
  labelSetText falsePositives (show $ fp matData)
  labelSetText trueNegatives (show $ tn matData)
  labelSetText accuracy ((show $ acc matData) ++ "%")


-- evaluateNetwork :: IO ()
evaluateNetwork datafile networkTextView algorithmVersion
                initialSeparator fillInMatrix fillInMatrixVal putOut = do
  filename <- readIORef datafile

  chosenAlgorithm <- comboBoxGetActive algorithmVersion
                     
  chosenSeparator <- comboBoxGetActive initialSeparator
  let sep = if chosenSeparator == 2 then centroidSeparator else noSeparator
  (tm, vm, network) <- if chosenAlgorithm == 1 then trainNetwork filename (createNetwork sep) putOut
                       else trainNetwork filename (TrainingOld.createNetwork sep) putOut
  
  fillInMatrix tm
  fillInMatrixVal vm

  buffer <- textViewGetBuffer networkTextView
  textBufferSetText buffer (show (net network))

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


putStrLnToTextView :: TextView -> String -> IO ()
putStrLnToTextView consoleTextView text = do
  buffer <- textViewGetBuffer consoleTextView
  mark <- textBufferGetInsert buffer
  iter <- textBufferGetIterAtMark buffer mark
  textBufferInsert buffer iter (text ++ "\n")

main = do

  -- Create a variable to store the path to the data set later on
  datafile <- newIORef ""

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

  -- Confusion matrix labels (training)
  truePositives <- builderGetObject builder castToLabel "truePositives"
  falseNegatives <- builderGetObject builder castToLabel "falseNegatives"
  falsePositives <- builderGetObject builder castToLabel "falsePositives"
  trueNegatives <- builderGetObject builder castToLabel "trueNegatives"
  accuracy <- builderGetObject builder castToLabel "accuracy"

  -- Confusion matrix labels (validation)
  truePositivesVal <- builderGetObject builder castToLabel "truePositivesVal"
  falseNegativesVal <- builderGetObject builder castToLabel "falseNegativesVal"
  falsePositivesVal <- builderGetObject builder castToLabel "falsePositivesVal"
  trueNegativesVal <- builderGetObject builder castToLabel "trueNegativesVal"
  accuracyVal <- builderGetObject builder castToLabel "accuracyVal"

  -- Menu items
  loadDataMenuItem <- builderGetObject builder castToMenuItem "loadDataMenuItem"
  saveNetworkMenuItem <- builderGetObject builder castToMenuItem "saveNetworkMenuItem"
  quitMenuItem <- builderGetObject builder castToMenuItem "quitMenuItem"
  helpMenu <- builderGetObject builder castToMenuItem "helpMenu"

  -- Text view
  networkTextView <- builderGetObject builder castToTextView "networkTextView"
  consoleTextView <- builderGetObject builder castToTextView "console"

  -- Add ways to exit the application
  window `on` deleteEvent $ liftIO mainQuit >> return False

  -- Set the combo boxes to default values
  comboBoxSetActive validationMethod 1
  comboBoxSetActive algorithmVersion 0
  comboBoxSetActive initialSeparator 0

  -- Create the function for confusion labels
  let fillInMatrix = displayConfusionMatrix truePositives falseNegatives
                     falsePositives trueNegatives accuracy
  let fillInMatrixVal = displayConfusionMatrix truePositivesVal falseNegativesVal
                        falsePositivesVal trueNegativesVal accuracyVal

  -- Connect signals
  quitMenuItem `on` menuItemActivated $ mainQuit
  regenerate `on` buttonActivated $ (evaluateNetwork datafile networkTextView algorithmVersion
                                                     initialSeparator fillInMatrix fillInMatrixVal
                                                                          (putStrLnToTextView consoleTextView))
  loadDataMenuItem `on` menuItemActivated $ (chooseDataset datafile window) >> (buttonClicked regenerate)

  -- Disable the elements not currently used
  widgetSetSensitive validationMethod False
  widgetSetSensitive saveNetworkMenuItem False
  widgetSetSensitive helpMenu False
  
  -- Display GUI and run the main application
  widgetShowAll window
  putStrLnToTextView consoleTextView "Initialisation finished."
  mainGUI
