\documentclass[11pt,a4paper]{article}

\usepackage{amsmath}

\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      basicstyle=\small\ttfamily,
      flexiblecolumns=false,
      basewidth={0.5em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2 
               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1               
    }
        
\begin{document}
\section{Import statements}
\begin{code}
import Data.List
import Data.Function

-- Only needed by the Misc section
import Text.ParserCombinators.Parsec
import Data.List
import Data.Maybe
import System.Random
import System.Environment

-- Needed for debugging
import GHC.Stack
\end{code}
\section{Basic datatypes}
In this section, we define some basic datatypes to support the construction of our neural network.

We start by defining the Input and Classification types. An Input is an n-dimentional list of Doubles that is used to represent the position of a given data point. A Classification is either +1.0 or -1.0. We represent it as a double since it is a result of mathematical manipulation on Double values, but it should never be anything other than $\pm$1.

\begin{code}
type Input = [Double]
type Classification = Double
\end{code}

Next, we define a Network datatype. It is defined inductively as one of the following possible constructions:

\begin{description}
  \item[An empty network] \hfill \\
  This type of network exists solely to be a starting point for recursion in the algorithms outlined below. It does not represent a valid network, with its only property being that a union or intersection of such a network with another network leaves the other network unchanged.
  \item[A separating hyperplane] \hfill \\
  This is the most basic type of network. It operates on two inputs, a point classified as +1 and a point classified as -1. It also accepts a parameter, c. It then constructs a separating hyperplane such that, if it were a line in 2 dimentions, it would be orthogonal to the line connecting the +1 and -1 points. The parameter c determines the exact point along this connecting line at which the separating hyperplane intersects with it, with 0.5 being exatly in the middle between the two given points.

  Mathematically, this can be expressed as

  \begin{displaymath}
    <x, v - u> - c||v - u||^2 + ||u||^2 - <u,v> = 0
  \end{displaymath}

  Where $x$ is the input, and $u$ and $v$ are -1 and +1 points, respectively. The parameter $c$, as described above, is used to determine the "closeness" of the line to one of the inputs.
  \item[A union of two networks] \hfill \\
  A union of two networks, with the sign function as the activation function, can be defined as
  \begin{displaymath}
  n_1 \cup n_2 = sign\left(n_1 + n_2 + \frac{1}{2}\right)
  \end{displaymath}
  \item[An intersection of two networks] \hfill \\
  An intersection of two networks, with the sign function as the activation function, can be defined as
  \begin{displaymath}
  n_1 \cap n_2 = sign\left(n_1 + n_2 - \frac{1}{2}\right)
  \end{displaymath}
\end{description}

Given the definitions above, we can express Network as a datatype:

\begin{code}
data Network = Empty
             | Hyperplane { plusPoint :: Input
                          , minusPoint :: Input
                          , c :: Double
                          }
             | Union Network Network
             | Intersection Network Network
               deriving (Show, Eq)
\end{code}

We also introduce a type synonym NetworkFunction, which is the actual function used to classify an input, simply to be:

\begin{code}
type NetworkFunction = Input -> Classification
\end{code}

Finally, we introduce the sign function that will serve as the activation function for a constructed network:

\begin{code}
sign :: Double -> Classification
sign = signum
\end{code}

We can now define a function, makeNetwork, that will transform our Network into a NetworkFunction that can be used for classification.

We use pattern matching and the definition of a network outlined above. 

\begin{code}
makeNetwork :: Network -> NetworkFunction
makeNetwork Empty = errorWithStackTrace "Attempted to construct an empty network"
                    
makeNetwork (Hyperplane plusOne minusOne c) = sign . sepFunct minusOne plusOne c
                                              
makeNetwork (Union Empty Empty) = error "Union of 2 empty networks"
makeNetwork (Union Empty n) = makeNetwork n
makeNetwork (Union n Empty) = makeNetwork n
makeNetwork (Union n1 n2) = \x -> sign $ (n1' x) + (n2' x) + 0.5
    where
    n1' = makeNetwork n1
    n2' = makeNetwork n2
          
makeNetwork (Intersection Empty Empty) = error "Intersection of 2 empty networks"
makeNetwork (Intersection Empty n) = makeNetwork n
makeNetwork (Intersection n Empty) = makeNetwork n
makeNetwork (Intersection n1 n2) = \x -> sign $ (n1' x) + (n2' x) - 0.5
    where
    n1' = makeNetwork n1
    n2' = makeNetwork n2

sepFunct :: Input -> Input -> Double -> (Input -> Double)
sepFunct u v c = \x -> (x <.> w) - l
    where
    squaredNorm = sum . map (^2)
    a <.> b = sum $ zipWith (*) a b
    w = zipWith (-) v u
    l = c * (squaredNorm w) - (squaredNorm u) + (u <.> v)
\end{code}

The function sepFunct is based on the mathematical definition of a separating hyperplane above.

\section{Preparing the inputs}

Before we construct a network, it would be helpful to get the inputs into a concise format.

We assume that we are given a list of inputs to use in the construction of our network. In keeping with the neural networks vocabulary, we will refer to them as "training" inputs, even though it is more appropriate to say that our network is being constructed, rather than trained.

Each such input is assigned a classification that it should be classed as, and a list of such pre-classified inputs will be refered to as the "training set".

\begin{code}
type TrainingInput = (Input, Classification)
type TrainingSet = [TrainingInput]
\end{code}

There are various ways in which we can use this information to construct a neural network. We will provide a generic groupInputs function that will group the inputs by their classification (i.e. +1s together with +1s, and -1s together with -1s). It will take, as an argument, another function (a "group" function), which can be used to group inputs together based on some criteria. We also provide a simple group function that sorts inputs based on distance, thus letting us compare closest points first.

We then return a list of tuples, one for each +1 point. The first element of the tuple would be said +1 point, with the second element being a list of all possible minus points, sorted by distance to the given +1 point. This list of tuples can then be iterated over to construct the network.

\begin{code}
type GroupedInputs = [(TrainingInput, [TrainingInput])]
type GroupFunction = [TrainingInput] -> [TrainingInput] -> GroupedInputs

groupInputs :: GroupFunction -> TrainingSet -> GroupedInputs
groupInputs f is = let [xs, ys] = groupInputs' is
                  in f ys xs

groupInputs' :: TrainingSet -> [[TrainingInput]]
groupInputs' = groupBy ((==) `on` snd) . sortBy (compare `on` snd)

distance :: Input -> Input -> Double
distance x y = sqrt $ sum $ map (^2) $ zipWith (-) x y
               
distanceCompare :: TrainingInput -> TrainingInput -> TrainingInput -> Ordering
distanceCompare x y1 y2 = distance (fst x) (fst y1) `compare` distance (fst x) (fst y2)

distanceGroup :: GroupFunction
distanceGroup xs ys = map pairWithYs xs
    where
    pairWithYs :: TrainingInput -> (TrainingInput, [TrainingInput])
    pairWithYs x = let ys' = sortBy (distanceCompare x) ys
                   in (x, ys')


prepareInputs :: TrainingSet -> GroupedInputs
prepareInputs = groupInputs distanceGroup
\end{code}
        
\section{Network construction}

We now begin the network construction mechanism. Firstly, we define a seave function that will take a Network and GroupedInputs, and will return any inputs that are misclassified by a given network. This way, we can concentrate on the inputs for which our network needs adjusting, and not worry too much about those that are already correctly classified.

\begin{code}
seave :: Network -> (TrainingInput, [TrainingInput]) -> (TrainingInput, [TrainingInput])
seave Empty i = i
seave net (x, ys) = (x, filter misclassified ys)
    where
    misclassified :: TrainingInput -> Bool
    misclassified (point, c) = ((makeNetwork net) point) /= c
\end{code}
        
We can now turn to the network construction code. We begin with an empty network and a list of +1 points, matched against all -1 points. Then:

\begin{itemize}
  \item For each +1 point:
  \begin{itemize}
     \item Filter out the points already correctly classified
     \item Create a subnetwork, such that:
     \begin{itemize}
       \item For the first misclassified -1 point, construct a separating hyperplane and intersect it with the subnetwork constructed so far
     \end{itemize}
     \item If the union of this subnetwork and the network constructed so far correctly classifies all of the remaining -1 points for this +1 point, return this union
     \item Otherwise, amend the subnetwork as above, via intersection, and check again
   \end{itemize}
   \item When there are no more +1 points with misclassified -1 points left, we are done
\end{itemize}

Found possible source of the bug: we are currently not checking for misclassified +1 points.

Our network might need to carry out additional checking for those cases.

We now define functions to carry out the procedures above (for now, we fix the parameter c at 0.5):

\begin{code}
createSubnet :: Network -> (TrainingInput, TrainingInput) -> Network
createSubnet subnet (x, y) = Intersection subnet (Hyperplane (fst x) (fst y) 0.5)

augmentPlusNet :: Network -> Network -> (TrainingInput, [TrainingInput]) -> Network
augmentPlusNet net subnet (x, []) = Union net subnet
augmentPlusNet Empty subnet (x, (y:ys)) = augmentPlusNet subnet' Empty (seave subnet' (x, ys))
    where
      subnet' = createSubnet subnet (x, y)
augmentPlusNet net subnet (x, (y:ys))
    = augmentPlusNet net subnet' (seave union (x, ys))
      where
        subnet' :: Network
        subnet' = createSubnet subnet (x, y)
        union :: Network
        union = Union net subnet'

createNetwork :: Network -> GroupedInputs -> Network
createNetwork net [] = net
createNetwork net (i:is) = createNetwork (augmentPlusNet net Empty i') is
    where
    i' = seave net i
\end{code}

\section{Misc}
The following code is only present to make it easier to test the code, giving it a main function and command line arguments, as well as the ability to parse CSV files.
        
\begin{code}
data TrainingData = TrainingData { inputs :: [Double]
                                 , cl :: Class
                                 }
                    deriving Show
type Class = String
type ClassMap = [(String, Double)]

csvFile = do
  result <- many line
  eof
  return result

-- Based on real world haskell
line :: GenParser Char st [String]
line = do result <- cells
          eol
          return result

cells = do first <- cellContent
           rest <- remainingCells
           return (first : rest)

remainingCells = (char ',' >> cells) <|> return ([])

cellContent = many (noneOf ",\n")

eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

-- Read coordinates in as Doubles and separate the classification at
-- the end
prepareData' :: [[String]] -> [TrainingData]
prepareData' = filter (not . empty) . map toDataPoint
    where
      empty :: TrainingData -> Bool
      empty td = length (inputs td) == 0
      toDataPoint :: [String] -> TrainingData
      toDataPoint ds = TrainingData inputs cl
          where
            cl = head $ reverse $ ds
            inputs = map read (reverse $ drop 1 $ reverse ds)

-- Make a list of unique classes that are present in the input data
uniqueClasses :: [TrainingData] -> [String]
uniqueClasses = nub . map cl

-- Convert the classes to +1/-1
numericClasses :: ClassMap -> TrainingData -> TrainingInput
numericClasses classMap dt = (inputs dt, fromJust $ lookup (cl dt) classMap)

-- Prepare data for use. Convert classes to numbers, and report errors
-- if more than 2 distinct classes are present
prepareData :: [[String]] -> Either String (ClassMap, TrainingSet)
prepareData parsed = let dt = prepareData' parsed
                         classes = uniqueClasses dt
                     in if (length classes) /= 2 then
                            Left "Need 2 unique classes to work with"
                        else
                            Right $ ((zip classes [1.0,(-1.0)]),
                                     map (numericClasses
                                          (zip classes [1.0,(-1.0)]))
                                     dt)
                                     
-- Read a file in and prepare data for use
readCSVData :: String -> IO (Either String (ClassMap, TrainingSet))
readCSVData path = do
  contents <- readFile path
  case (parseCSV contents) of
    Left err -> return $ Left (show err)
    Right dt -> return $ prepareData dt

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

usage :: IO ()
usage = do
  progname <- getProgName
  putStrLn ("Usage: " ++ progname ++ " training-perc CSV-input-file")

testOn :: String -> Int -> IO ()
testOn filename p = do
  dt <- readCSVData filename
  case dt of
    Left err -> putStrLn err
    Right (classMap, dataSet) -> do
                    let class1 = classMap !! 0
                    let class2 = classMap !! 1
                    putStrLn ("Assigning classes: " ++ (show class1) ++ ", " ++ (show class2))
                    gen <- getStdGen
                    let splitList = splitList' p
                    let (training, verification) = splitList gen dataSet
                    let network = createNetwork Empty (prepareInputs training)
                    let results = map (\(x, y) -> x == y) $ map (\(x,y) -> ((makeNetwork network) x, y)) verification
                    let errors = length $ filter (==False) results
                    putStrLn ("After training on " ++ (show p) ++ "% of the data set, there were " ++ (show errors) ++ " errors during verification.")
                    putStrLn "Verified on the following values: "
                    mapM_ putStrLn $ map show verification
                    putStrLn "The resulting network is as follows:"
                    putStrLn $ show network
                             
  
main :: IO ()
main = do
  args <- getArgs
  if (length args) /= 2 then usage else testOn (args !! 1) (read (args !! 0))
  
\end{code}
\end{document}
