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
import Training
import Parsing
import Types
import Networks
import InitialSeparators
import Validation
import ConfusionMatrix
import Plotting

-- | A helper data structure for passing the necessary elements of the GUI
-- to different parts of the module
data GUI = GUI { consoleOut :: (String -> IO()) -- ^ a function to print to the console
               , displayTrainingMatrix :: (ConfusionMatrix -> IO ()) -- ^ a function to set the training confusion matrix lables correctly
               , displayValidationMatrix :: (ConfusionMatrix -> IO ()) -- ^ a function to set the validation confusion matrix lables correctly
               , displayNetwork :: (Network -> IO ()) -- ^ a function to display the textual representation of the network to the user
               , displayPerceptronCount :: (Network -> IO ()) -- ^ a function to display perceptron count of the network to the user
               , dataFile :: IORef FilePath -- ^ the path to the data file being used currently
               , initialSeparator :: ComboBox 
               , validationMethod :: ComboBox
               , visualisationFlag :: CheckButton
               , plotArea :: ScrolledWindow -- ^ an area into which the plots should be inserted
               , rootWindow :: Window -- ^ the main application window
               }

-- * Combo box choice maps
separatorMap :: Map Int SeparatorFunction
separatorMap = fromList [ (0, noSeparator)
                        , (1, centroidSeparator)
                        ]

validationMap :: Map Int ValidationFunction
validationMap = fromList [ (0, crossValidation 10)
                         , (1, splitValidation 70)
                         ]

-- | Uses the path stored in 'dataFile' to read in training data
readTrainingSet :: GUI -> IO (ClassMap, TrainingSet)
readTrainingSet gui = do
  filename <- readIORef (dataFile gui)
  dataContents <- readCSVData filename

  case dataContents of
    Left err -> (consoleOut gui) err >> return ([], [])
    Right parsed -> return parsed

-- | Uses the 'ComboBox' fields in the 'GUI' to pick the appropriate functions from the 'Map's
-- provided, and trains a network, using a specified validation method.
-- The training and validation confusion matrices are then returned, along with a trained network.
trainNetwork :: GUI -> IO (ConfusionMatrix, ConfusionMatrix, Network)
trainNetwork gui = do
  chosenSeparator <- comboBoxGetActive $ initialSeparator gui
  chosenValidation <- comboBoxGetActive $ validationMethod gui
                     
  let sep = separatorMap ! chosenSeparator
  let validationMethod = validationMap ! chosenValidation

  (classMap, trainingSet) <- readTrainingSet gui

  case trainingSet of
    [] -> return (emptyConfusionMatrix, emptyConfusionMatrix, emptyNet)
    _ -> do
      let class1 = classMap !! 0
      let class2 = classMap !! 1
      (consoleOut gui) ("Assigning classes: " ++ (show class1) ++ ", " ++ (show class2))
      gen <- getStdGen
      return $ validationMethod gen trainingSet (createNetwork sep)

-- | Displays a confusion matrix. Uses the 'Builder' object to extract the labels
-- with a given prefix. The prefix is assumed to be absent for the labels of the training
-- matrix, and be "val" for the labels of the validation matrix.
displayConfusionMatrix :: Builder
                       -> String -- ^ a prefix for labels
                       -> ConfusionMatrix
                       -> IO ()
displayConfusionMatrix builder suffix matrix = do
  (getCorrectLabel "truePositives") >>= \l -> labelSetText l (show $ truePositives matrix)
  (getCorrectLabel "trueNegatives") >>= \l -> labelSetText l (show $ trueNegatives matrix)
  (getCorrectLabel "falsePositives") >>= \l -> labelSetText l (show $ falsePositives matrix)
  (getCorrectLabel "falseNegatives") >>= \l -> labelSetText l (show $ falseNegatives matrix)
  (getCorrectLabel "accuracy") >>= \l -> labelSetText l (show $ accuracy matrix)
    where
      getCorrectLabel baseName = builderGetObject builder castToLabel (baseName ++ suffix)

-- | Remove any plots currently present in the UI
clearPlots :: GUI -> IO ()
clearPlots gui = do
  [child] <- containerGetChildren $ plotArea gui
  widgetDestroy child

-- | Should be called when the plots either couldn't be constrcuted or
-- the user choose not to generate them.
noPlots :: GUI -> IO ()
noPlots gui = do
  label <- labelNew $ Just "Plots disabled"
  scrolledWindowAddWithViewport (plotArea gui) label
  widgetShowAll $ plotArea gui

-- | Plot a network and place it in container specified by 'plotArea'
generatePlots :: GUI -> Network -> IO ()
generatePlots gui network = do
  (classMap, trainingSet) <- readTrainingSet gui
  case trainingSet of
    [] -> noPlots gui
    _ -> do
      plots <- generateAllProjections trainingSet classMap network
      (plotArea gui) `scrolledWindowAddWithViewport` plots
      widgetShowAll $ plotArea gui


-- | Display plots if the user ticked the box, and clear the plot area
-- otherwise
displayPlots :: GUI -> Network -> IO ()
displayPlots gui network = do
  buttonState <- toggleButtonGetActive $ visualisationFlag gui
  clearPlots gui
  if buttonState then generatePlots gui network else noPlots gui

-- | The main function, executed every time a new data set is loaded or the
-- user clicks the 're-generate' button. Trains the network, displays the confusion
-- matrix and the plots, if user chose to generate them.
evaluateNetwork :: GUI -> IO ()
evaluateNetwork gui = do
  (tm, vm, network) <- trainNetwork gui
  
  (displayTrainingMatrix gui) tm
  (displayValidationMatrix gui) vm

  (displayPlots gui) network

  (displayNetwork gui) network

  (displayPerceptronCount gui) network

-- | Presents the user with a file chooser dialogue. If a file is picked, it is set
-- as the data source.
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


-- | A function to print a message to the "console" inside the GUI.
-- The 'TextView' provided should be the "console" window.
putStrLnToTextView :: TextView -> String -> IO ()
putStrLnToTextView consoleTextView text = do
  buffer <- textViewGetBuffer consoleTextView
  mark <- textBufferGetInsert buffer
  iter <- textBufferGetIterAtMark buffer mark
  textBufferInsert buffer iter (text ++ "\n")

-- | A function to print the textual representation of the network
-- not the 'TextView' provided.
displayNetworkToTextView :: TextView -> Network -> IO ()
displayNetworkToTextView textView network = do
  buffer <- textViewGetBuffer textView
  textBufferSetText buffer (show network)

-- | Sets the supplied 'Label' to the number representing
-- the number of perceptrons in the network
displayPCountToLabel :: Label -> Network -> IO ()
displayPCountToLabel label network = do
  let pCount = countPerceptrons network
  labelSetText label (show pCount)

-- | Prepare a 'GUI' object based on the XML file provided
prepareGui :: FilePath -> IO GUI
prepareGui builderPath = do
  -- Create a variable to store the path to the data set later on
  datafile <- newIORef ""

  -- Initialise the GUI
  initGUI
  builder <- builderNew
  builderAddFromFile builder builderPath

  -- Get the named items
  -- The main window
  window <- builderGetObject builder castToWindow "mainWindow"

  -- Combo boxes
  validationMethodCombo <- builderGetObject builder castToComboBox "validationMethod"
  initialSeparatorCombo <- builderGetObject builder castToComboBox "initialSeparator"

  -- Buttons
  regenerate <- builderGetObject builder castToButton "regenerate"

  -- Menu items
  loadDataMenuItem <- builderGetObject builder castToMenuItem "loadDataMenuItem"
  quitMenuItem <- builderGetObject builder castToMenuItem "quitMenuItem"

  -- Text view
  networkTextView <- builderGetObject builder castToTextView "networkTextView"
  consoleTextView <- builderGetObject builder castToTextView "console"

  -- Checkbox
  visualisationCheckBox <- builderGetObject builder castToCheckButton "visualiseCheckBox"

  -- Scrolled window
  plotAreaWindow <- builderGetObject builder castToScrolledWindow "plotArea"

  -- Label
  perceptronCountLabel <- builderGetObject builder castToLabel "percepCount"

  -- Add ways to exit the application
  window `on` deleteEvent $ liftIO mainQuit >> return False

  -- Set the combo boxes to default values
  comboBoxSetActive validationMethodCombo 0
  comboBoxSetActive initialSeparatorCombo 0

  let guiConfig = GUI { consoleOut = putStrLnToTextView consoleTextView
                      , displayTrainingMatrix = displayConfusionMatrix builder ""
                      , displayValidationMatrix = displayConfusionMatrix builder "Val"
                      , displayNetwork = displayNetworkToTextView networkTextView
                      , displayPerceptronCount = displayPCountToLabel perceptronCountLabel
                      , dataFile = datafile
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

  return guiConfig
