{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module AI.Search.Example.Graph where

import Control.DeepSeq
import Control.Monad.State (StateT)
import Control.Monad
import Data.IORef
import Data.Map (Map, (!))
import Data.Maybe (fromJust)
import System.IO
import System.IO.Unsafe

import qualified Control.Monad.State as State
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O
import qualified System.Random as R

import AI.Search.Core
import AI.Search.Uninformed
import AI.Search.Informed
import AI.Util.WeightedGraph (WeightedGraph)
import AI.Util.Table
import AI.Util.Util

import qualified AI.Util.WeightedGraph as G
import AI.Util.GraphMap

-- |Construct a random graph with the specified number of nodes, and random
--  links. The nodes are laid out randomly on a @(width x height)@ rectangle.
--  Then each node is connected to the @minLinks@ nearest neighbours. Because
--  inverse links are added, some nodes will have more connections. The distance
--  between nodes is the hypotenuse multiplied by @curvature@, where @curvature@
--  defaults to a random number between 1.1 and 1.5.
randomGraphMap ::
                 Int    -- ^ Number of nodes
              -> Int    -- ^ Minimum number of links
              -> Double -- ^ Width
              -> Double -- ^ Height
              -> IO (GraphMap Int)
randomGraphMap n minLinks width height = State.execStateT go (mkGraphMap [] []) where
    go = do
        replicateM n mkLocation >>= State.put . mkGraphMap [] . zip nodes

        forM_ nodes $ \x -> do

            State.modify (addEmpty x)
            g @ (G _ loc) <- State.get

            let nbrs     = map fst (getNeighbours x g)
                numNbrs  = length nbrs

                unconnected = deleteAll (x:nbrs) nodes
                sorted      = L.sortBy (O.comparing to_x) unconnected
                to_x y      = euclideanDist (loc ! x) (loc ! y)
                toAdd       = take (minLinks - numNbrs) sorted

            mapM_ (addLink x) toAdd

        where
            nodes = [1..n]

            addLink x y = do
                curv <- curvature
                dist <- distance x y
                State.modify $ addEdge x y (dist * curv)

            addEmpty x (G graph xs) = G (M.insert x M.empty graph) xs

            mkLocation = State.liftIO $ do
                x <- R.randomRIO (0,width)
                y <- R.randomRIO (0,height)
                return (x,y)

            curvature = State.liftIO $ R.randomRIO (1.1, 1.5)

            distance x y = do
                (G _ loc) <- State.get
                return $ euclideanDist (loc ! x) (loc ! y)

-- |Return a random instance of a graph problem with the specified number of
--  nodes and minimum number of links.
randomGraphProblem :: Int -> Int -> IO (GraphProblem Int Int)
randomGraphProblem numNodes minLinks = do
    g@(G _ loc) <- randomGraphMap numNodes minLinks 100 100
    let initial = fst $ L.minimumBy (O.comparing (fst.snd)) (M.toList loc)
        goal    = fst $ L.maximumBy (O.comparing (fst.snd)) (M.toList loc)
    return (GP g initial goal)

-- |Write a list of graph problems to a file.
writeGraphProblems :: Show p => FilePath -> [p] -> IO ()
writeGraphProblems filename ps = do
    h <- openFile filename WriteMode
    forM_ ps (hPrint h)
    hClose h

-- |Read a list of graph problems from a file.
readGraphProblems :: FilePath -> IO [GraphProblem Int Int]
readGraphProblems filepath = do
    contents <- readFile filepath
    return $ map read $ lines contents

-- |Generate random graph problems and write them to a file. Each problem is
--  checked for solvability by running the 'depthFirstGraphSearch' algorithm
--  on it. This function finds poor solutions, but terminates quickly on this
--  kind of problem.
generateGraphProblems :: Int -> Int -> Int -> FilePath -> IO ()
generateGraphProblems numProbs numNodes minLinks filepath = do
    probs <- go numProbs
    writeGraphProblems filepath probs
    where
        go 0 = return []
        go n = do
            p  <- randomGraphProblem numNodes minLinks
            case depthFirstGraphSearch p of
                Nothing -> go n
                Just _  -> go (n-1) >>= \ps -> return (p:ps)

----------------------------------
-- Graphs used in AIMA examples --
----------------------------------

-- |The Romania graph from AIMA.
romania :: GraphMap String
romania = mkGraphMap

    [ ("A", [("Z",75), ("S",140), ("T",118)])
    , ("B", [("U",85), ("P",101), ("G",90), ("F",211)])
    , ("C", [("D",120), ("R",146), ("P",138)])
    , ("D", [("M",75)])
    , ("E", [("H",86)])
    , ("F", [("S",99)])
    , ("H", [("U",98)])
    , ("I", [("V",92), ("N",87)])
    , ("L", [("T",111), ("M",70)])
    , ("O", [("Z",71), ("S",151)])
    , ("P", [("R",97)])
    , ("R", [("S",80)])
    , ("U", [("V",142)]) ]

    [ ("A",( 91,491)), ("B",(400,327)), ("C",(253,288)), ("D",(165,299))
    , ("E",(562,293)), ("F",(305,449)), ("G",(375,270)), ("H",(534,350))
    , ("I",(473,506)), ("L",(165,379)), ("M",(168,339)), ("N",(406,537))
    , ("O",(131,571)), ("P",(320,368)), ("R",(233,410)), ("S",(207,457))
    , ("T",( 94,410)), ("U",(456,350)), ("V",(509,444)), ("Z",(108,531)) ]

-- |The Australia graph from AIMA.
australia :: GraphMap String
australia = mkGraphMap

    [ ("T",   [])
    , ("SA",  [("WA",1), ("NT",1), ("Q",1), ("NSW",1), ("V",1)])
    , ("NT",  [("WA",1), ("Q",1)])
    , ("NSW", [("Q", 1), ("V",1)]) ]

    [ ("WA",(120,24)), ("NT" ,(135,20)), ("SA",(135,30)),
      ("Q" ,(145,20)), ("NSW",(145,32)), ("T" ,(145,42)), ("V",(145,37))]

gp1, gp2, gp3  :: GraphProblem String String
gp1 = GP { graphGP = australia, initGP = "Q", goalGP = "WA" }
gp2 = GP { graphGP = romania, initGP = "A", goalGP = "B" }
gp3 = GP { graphGP = romania, initGP = "O", goalGP = "N" }

-----------------------------
-- Compare Graph Searchers --
-----------------------------

-- |Run all search algorithms over a particular problem and print out
--  performance statistics.
runDetailedCompare :: (Problem p s a, Ord s, Show s) => p s a -> IO ()
runDetailedCompare = detailedCompareSearchers allSearchers allSearcherNames

-- |List of all search algorithms that can be applied to problems with a graph
--  structure. I'd like to add an iterative deepening graph search to this list,
--  as well as some of the more exotic search algorithsm described in the
--  textbook.
allSearchers :: (Problem p s a, Ord s) => [p s a -> Maybe (Node s a)]
allSearchers = [ breadthFirstGraphSearch, depthFirstGraphSearch
               , greedyBestFirstSearch, uniformCostSearch, aStarSearch']

-- |Names for the search algorithms in this module.
allSearcherNames :: [String]
allSearcherNames = [ "Breadth First Graph Search" , "Depth First Graph Search"
                   , "Greedy Best First Search", "Uniform Cost Search"
                   , "A* Search"]

-----------
-- Demos --
-----------

-- |Run all search algorithms over a few example problems.
demo1 :: IO ()
demo1 = compareSearchers allSearchers probs header allSearcherNames >> return ()
    where
        probs     = [gp1, gp2, gp3]
        header    = ["Searcher", "Australia", "Romania(A,B)","Romania(O,N)"]

-- |Load ten example problems from a file and run all searchers over them.
demo2 :: IO ()
demo2 = do
    ps <- readGraphProblems "data/problems_small.txt"
    mapM_ runDetailedCompare ps

-- |Load 100 example problems from a file and run all searchers over them.
demo3 :: IO ()
demo3 = do
    ps <- readGraphProblems "data/problems_large.txt"
    mapM_ runDetailedCompare ps
