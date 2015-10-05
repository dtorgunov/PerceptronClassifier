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
makeNetwork Empty = error "Attempted to construct an empty network"
                    
makeNetwork (Hyperplane plusOne minusOne c) = sign . sepFunct minusOne plusOne c
                                              
makeNetwork (Union Empty n) = makeNetwork n
makeNetwork (Union n Empty) = makeNetwork n
makeNetwork (Union n1 n2) = \x -> sign $ (n1' x) + (n2' x) + 0.5
    where
    n1' = makeNetwork n1
    n2' = makeNetwork n2
          
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

\end{document}
