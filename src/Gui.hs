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

-- Take n unique elements from a given list
takeUnique :: (Eq a) => Int -> [a] -> [a] -> [a]
takeUnique 0 _ acc = acc
takeUnique n (i:ins) acc | i `elem` acc = takeUnique n ins acc
                         | otherwise = takeUnique (n-1) ins (i:acc)

-- Take the elements at indexes in is from the list of xs
takeIndexes :: [Int] -> [a] -> [a] -> [a]
takeIndexes [] _ acc = acc
takeIndexes (i:is) xs acc = takeIndexes is xs ((xs !! i):acc)
                                       

-- A function to fascilitate verification.
-- Given a list, split it roughly p/(100 - p)
splitList' :: (RandomGen g, Eq a) => Int -> g -> [a] -> ([a], [a])
splitList' 100 _ xs = (xs, xs)
splitList' p g xs = (training, verification)
    where
      indexes = takeUnique ((length xs - 1) * p `div` 100) (randomRs (0, (length xs - 1)) g) []
      training = takeIndexes indexes xs []
      verification = xs \\ training

trainNetwork :: FilePath -> (TrainingSet -> Network) -> IO (TrainingSet, TrainingSet, Network)
trainNetwork filename trainingMethod = do
  dt <- readCSVData filename
  case dt of
    Left err -> putStrLn err >> return ([],[],(makeNetwork Empty))
    Right (classMap, dataSet) -> do
                          let class1 = classMap !! 0
                          let class2 = classMap !! 1
                          putStrLn ("Assigning classes: " ++ (show class1) ++ ", " ++ (show class2))
                          gen <- getStdGen
                          -- 70/30 training
                          let splitList = splitList' 70
                          let (training, verification) = splitList gen dataSet
                          let network = trainingMethod training
                          return (training, verification, network)

-- Set the labels based on results
displayConfusionMatrix :: (LabelClass l) => l -> l -> l -> l -> l -> [(Double, Double)] -> IO ()
displayConfusionMatrix truePositives falseNegatives falsePositives trueNegatives accuracy results
    = do
  let tp = length $ filter (\(x,y) -> (x == 1) && (y == 1)) results
  let tn = length $ filter (\(x,y) -> (x == (-1)) && (y == (-1))) results
  let fp = length $ filter (\(returned, expected) -> ((returned == 1)
                                                     && (expected == (-1)))) results
  let fn = length $ filter (\(x,y) -> (x== (-1)) && (y == 1)) results
  labelSetText truePositives (show tp)
  labelSetText falseNegatives (show fn)
  labelSetText falsePositives (show fp)
  labelSetText trueNegatives (show tn)
  labelSetText accuracy (((show $ accuracyMeasure tp tn (length results))) ++ "%")

accuracyMeasure :: Int -> Int -> Int -> Double
accuracyMeasure truePositives trueNegatives total
    = (fromIntegral (truePositives+trueNegatives))*100.0/ (fromIntegral total)

-- evaluateNetwork :: IO ()
evaluateNetwork datafile networkTextView algorithmVersion
                initialSeparator fillInMatrix fillInMatrixVal = do
  filename <- readIORef datafile

  chosenAlgorithm <- comboBoxGetActive algorithmVersion
                     
  chosenSeparator <- comboBoxGetActive initialSeparator
  let sep = if chosenSeparator == 2 then centroidSeparator else noSeparator
  (ts, vs, network) <- if chosenAlgorithm == 1 then trainNetwork filename (createNetwork sep)
                       else trainNetwork filename (TrainingOld.createNetwork sep)
  
  let resultsTraining = map (\(x,y) -> ((runNetwork network) x, y)) ts
  fillInMatrix resultsTraining

  let resultsVerification = map (\(x,y) -> ((runNetwork network) x, y)) vs
  fillInMatrixVal resultsVerification

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
                                                     initialSeparator fillInMatrix fillInMatrixVal)
  loadDataMenuItem `on` menuItemActivated $ (chooseDataset datafile window) >> (buttonClicked regenerate)

  -- Disable the elements not currently used
  widgetSetSensitive validationMethod False
  widgetSetSensitive saveNetworkMenuItem False
  widgetSetSensitive helpMenu False
  
  -- Display GUI and run the main application
  widgetShowAll window
  mainGUI
