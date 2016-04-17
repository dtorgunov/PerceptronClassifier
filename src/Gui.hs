module Gui
    (
     prepareGui
    , rootWindow
    , consoleOut
    )where
    
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
import qualified Training.Version1 as Training1
import qualified Training.Version2 as Training2
import qualified Training.Version10 as Training3
import Parsing
import Types
import Networks
import InitialSeparators
import Validation
import ConfusionMatrix
import Plotting

-- A helper data structure for passing GUI elements around
data GUI = GUI { consoleOut :: (String -> IO())
               , displayTrainingMatrix :: (ConfusionMatrix -> IO ())
               , displayValidationMatrix :: (ConfusionMatrix -> IO ())
               , displayNetwork :: (Network -> IO ())
               , dataFile :: IORef FilePath
               , algorithmVersion :: ComboBox
               , initialSeparator :: ComboBox
               , validationMethod :: ComboBox
               , visualisationFlag :: CheckButton
               , plotArea :: ScrolledWindow
               , rootWindow :: Window
               }

-- Maps to implement the combo boxes
separatorMap :: Map Int SeparatorFunction
separatorMap = fromList [ (0, noSeparator)
                        , (2, centroidSeparator)
                        ]

algorithmMap :: Map Int (SeparatorFunction -> TrainingSet -> Network)
algorithmMap = fromList [ (0, Training1.createNetwork)
                        , (1, Training2.createNetwork)
                        , (2, Training3.createNetwork)
                        ]

validationMap :: Map Int ValidationFunction
validationMap = fromList [ (0, crossValidation 10)
                         , (1, splitValidation 70)]

readTrainingSet :: GUI -> IO (ClassMap, TrainingSet)
readTrainingSet gui = do
  filename <- readIORef (dataFile gui)
  dataContents <- readCSVData filename

  case dataContents of
    Left err -> (consoleOut gui) err >> return ([], [])
    Right parsed -> return parsed

trainNetwork :: GUI -> IO (ConfusionMatrix, ConfusionMatrix, Network)
trainNetwork gui = do
  chosenAlgorithm <- comboBoxGetActive $ algorithmVersion gui
  chosenSeparator <- comboBoxGetActive $ initialSeparator gui
  chosenValidation <- comboBoxGetActive $ validationMethod gui
                     
  let sep = separatorMap ! chosenSeparator
  let trainingMethod = algorithmMap ! chosenAlgorithm
  let validationMethod = validationMap ! chosenValidation

  (classMap, trainingSet) <- readTrainingSet gui

  case trainingSet of
    [] -> return (emptyConfusionMatrix, emptyConfusionMatrix, emptyNet)
    _ -> do
      let class1 = classMap !! 0
      let class2 = classMap !! 1
      (consoleOut gui) ("Assigning classes: " ++ (show class1) ++ ", " ++ (show class2))
      gen <- getStdGen
      return $ validationMethod gen trainingSet (trainingMethod sep)

displayConfusionMatrix :: Builder -> String -> ConfusionMatrix -> IO ()
displayConfusionMatrix builder suffix matrix = do
  (getCorrectLabel "truePositives") >>= \l -> labelSetText l (show $ truePositives matrix)
  (getCorrectLabel "trueNegatives") >>= \l -> labelSetText l (show $ trueNegatives matrix)
  (getCorrectLabel "falsePositives") >>= \l -> labelSetText l (show $ falsePositives matrix)
  (getCorrectLabel "falseNegatives") >>= \l -> labelSetText l (show $ falseNegatives matrix)
  (getCorrectLabel "accuracy") >>= \l -> labelSetText l (show $ accuracy matrix)
    where
      getCorrectLabel baseName = builderGetObject builder castToLabel (baseName ++ suffix)

clearPlots :: GUI -> IO ()
clearPlots gui = do
  [child] <- containerGetChildren $ plotArea gui
  widgetDestroy child

noPlots :: GUI -> IO ()
noPlots gui = do
  label <- labelNew $ Just "Plots disabled"
  scrolledWindowAddWithViewport (plotArea gui) label
  widgetShowAll $ plotArea gui

generatePlots :: GUI -> Network -> IO ()
generatePlots gui network = do
  (classMap, trainingSet) <- readTrainingSet gui
  case trainingSet of
    [] -> noPlots gui
    _ -> do
      plots <- generateAllProjections trainingSet classMap network
      (plotArea gui) `scrolledWindowAddWithViewport` plots
      widgetShowAll $ plotArea gui


displayPlots :: GUI -> Network -> IO ()
displayPlots gui network = do
  buttonState <- toggleButtonGetActive $ visualisationFlag gui
  clearPlots gui
  if buttonState then
      do
        generatePlots gui network
  else
      do
        noPlots gui

evaluateNetwork :: GUI -> IO ()
evaluateNetwork gui = do
  (tm, vm, network) <- trainNetwork gui
  
  (displayTrainingMatrix gui) tm
  (displayValidationMatrix gui) vm

  (displayPlots gui) network

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
  textBufferSetText buffer (show network)

prepareGui :: IO GUI
prepareGui = do
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

  -- Menu items
  loadDataMenuItem <- builderGetObject builder castToMenuItem "loadDataMenuItem"
  saveNetworkMenuItem <- builderGetObject builder castToMenuItem "saveNetworkMenuItem"
  quitMenuItem <- builderGetObject builder castToMenuItem "quitMenuItem"
  helpMenu <- builderGetObject builder castToMenuItem "helpMenu"

  -- Text view
  networkTextView <- builderGetObject builder castToTextView "networkTextView"
  consoleTextView <- builderGetObject builder castToTextView "console"

  -- Checkbox
  visualisationCheckBox <- builderGetObject builder castToCheckButton "visualiseCheckBox"

  -- Scrolled window
  plotAreaWindow <- builderGetObject builder castToScrolledWindow "plotArea"

  -- Add ways to exit the application
  window `on` deleteEvent $ liftIO mainQuit >> return False

  -- Set the combo boxes to default values
  comboBoxSetActive validationMethodCombo 0
  comboBoxSetActive algorithmVersionCombo 0
  comboBoxSetActive initialSeparatorCombo 0

  let guiConfig = GUI { consoleOut = putStrLnToTextView consoleTextView
                      , displayTrainingMatrix = displayConfusionMatrix builder ""
                      , displayValidationMatrix = displayConfusionMatrix builder "Val"
                      , displayNetwork = displayNetworkToTextView networkTextView
                      , dataFile = datafile
                      , algorithmVersion = algorithmVersionCombo
                      , initialSeparator = initialSeparatorCombo
                      , validationMethod = validationMethodCombo
                      , visualisationFlag = visualisationCheckBox
                      , plotArea = plotAreaWindow
                      , rootWindow = window
                      }
                  
  -- Connect signals
  quitMenuItem `on` menuItemActivated $ mainQuit
  regenerate `on` buttonActivated $ (evaluateNetwork guiConfig)
  loadDataMenuItem `on` menuItemActivated $ (chooseDataset guiConfig) >> (buttonClicked regenerate)

  -- Disable the elements not currently used
  widgetSetSensitive saveNetworkMenuItem False
  widgetSetSensitive helpMenu False

  return guiConfig
