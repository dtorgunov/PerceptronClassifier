module Main where

-- Data libraries
import Data.IORef
import Data.List
import Data.Map.Strict ((!),Map,fromList)

-- GTK Libraries
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
    
-- Other libraries
import Control.Monad.Trans(liftIO)
import System.Random

-- Project modules
import Training
import qualified TrainingOldAugmented as TrainingOld
import Parsing
import Types
import Networks
import InitialSeparators
import Validation

-- A helper data structure for passing GUI elements around
data GUI = GUI { consoleOut :: (String -> IO())
               , displayTrainingMatrix :: (ConfusionMatrixData -> IO ())
               , displayValidationMatrix :: (ConfusionMatrixData -> IO ())
               , displayNetwork :: (Network -> IO ())
               , dataFile :: IORef FilePath
               , algorithmVersion :: ComboBox
               , initialSeparator :: ComboBox
               , rootWindow :: Window
               }

-- Maps to implement the combo boxes
separatorMap :: Map Int SeparatorFunction
separatorMap = fromList [ (0, noSeparator)
                        , (2, centroidSeparator)
                        ]

algorithmMap :: Map Int (SeparatorFunction -> TrainingSet -> Network)
algorithmMap = fromList [ (0, TrainingOld.createNetwork)
                        , (1, createNetwork)
                        ]

trainNetwork :: GUI -> IO (ConfusionMatrixData, ConfusionMatrixData, Network)
trainNetwork gui = do
  filename <- readIORef (dataFile gui)
  dt <- readCSVData filename
        
  chosenAlgorithm <- comboBoxGetActive $ algorithmVersion gui
  chosenSeparator <- comboBoxGetActive $ initialSeparator gui
                     
  let sep = separatorMap ! chosenSeparator
  let trainingMethod = algorithmMap ! chosenAlgorithm

  case dt of
    Left err -> putStrLn err >> return (NoData, NoData, (makeNetwork Empty))
    Right (classMap, dataSet) -> do
                          let class1 = classMap !! 0
                          let class2 = classMap !! 1
                          (consoleOut gui) ("Assigning classes: " ++ (show class1) ++ ", " ++ (show class2))
                          gen <- getStdGen
                          -- 70/30 training
                          return $ splitValidation 70 gen dataSet (trainingMethod sep)

-- Set the labels based on results
displayConfusionMatrix :: (LabelClass l) => l -> l -> l -> l -> l -> ConfusionMatrixData -> IO ()
displayConfusionMatrix truePositives falseNegatives falsePositives trueNegatives accuracy matData
    = do
  labelSetText truePositives (show $ tp matData)
  labelSetText falseNegatives (show $ fn matData)
  labelSetText falsePositives (show $ fp matData)
  labelSetText trueNegatives (show $ tn matData)
  labelSetText accuracy ((show $ acc matData) ++ "%")

evaluateNetwork :: GUI -> IO ()
evaluateNetwork gui = do
  (tm, vm, network) <- trainNetwork gui
  
  (displayTrainingMatrix gui) tm
  (displayValidationMatrix gui) vm

  (displayNetwork gui) network

chooseDataset :: GUI -> IO ()
chooseDataset gui = do
  fcd <- fileChooserDialogNew (Just "Choose a data set") (Just $ rootWindow gui) FileChooserActionOpen
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
                  Just f -> writeIORef (dataFile gui) f

  widgetDestroy fcd
  return ()


putStrLnToTextView :: TextView -> String -> IO ()
putStrLnToTextView consoleTextView text = do
  buffer <- textViewGetBuffer consoleTextView
  mark <- textBufferGetInsert buffer
  iter <- textBufferGetIterAtMark buffer mark
  textBufferInsert buffer iter (text ++ "\n")

displayNetworkToTextView :: TextView -> Network -> IO ()
displayNetworkToTextView textView network = do
  buffer <- textViewGetBuffer textView
  textBufferSetText buffer (show (net network))

main :: IO ()
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
  validationMethodCombo <- builderGetObject builder castToComboBox "validationMethod"
  algorithmVersionCombo <- builderGetObject builder castToComboBox "algorithmVersion"
  initialSeparatorCombo <- builderGetObject builder castToComboBox "initialSeparator"

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
  comboBoxSetActive validationMethodCombo 1
  comboBoxSetActive algorithmVersionCombo 0
  comboBoxSetActive initialSeparatorCombo 0

  -- Create the function for confusion labels
  let fillInMatrix = displayConfusionMatrix truePositives falseNegatives
                     falsePositives trueNegatives accuracy
  let fillInMatrixVal = displayConfusionMatrix truePositivesVal falseNegativesVal
                        falsePositivesVal trueNegativesVal accuracyVal

  let guiConfig = GUI { consoleOut = putStrLnToTextView consoleTextView
                      , displayTrainingMatrix = fillInMatrix
                      , displayValidationMatrix = fillInMatrixVal
                      , displayNetwork = displayNetworkToTextView networkTextView
                      , dataFile = datafile
                      , algorithmVersion = algorithmVersionCombo
                      , initialSeparator = initialSeparatorCombo
                      , rootWindow = window
                      }
  -- Connect signals
  quitMenuItem `on` menuItemActivated $ mainQuit
  regenerate `on` buttonActivated $ (evaluateNetwork guiConfig)
  loadDataMenuItem `on` menuItemActivated $ (chooseDataset guiConfig) >> (buttonClicked regenerate)

  -- Disable the elements not currently used
  widgetSetSensitive validationMethodCombo False
  widgetSetSensitive saveNetworkMenuItem False
  widgetSetSensitive helpMenu False
  
  -- Display GUI and run the main application
  widgetShowAll window
  putStrLnToTextView consoleTextView "Initialisation finished."
  mainGUI
