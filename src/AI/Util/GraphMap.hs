{-# LANGUAGE FlexibleInstances #-}
module AI.Util.GraphMap where

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
-------------------------------
-- Graphs and Graph Problems --
-------------------------------

-- |Data structure to hold a graph (edge weights correspond to the distance
--  between nodes) and a map of graph nodes to locations.
data GraphMap a = G
    { getGraph     :: WeightedGraph a Cost
    , getLocations :: Map a Location } deriving (Show,Read)

-- |Type synonym for a pair of doubles, representing a location in cartesian
--  coordinates.
type Location = (Double,Double)

-- |Creates a GraphMap from the graph's adjacency list representation and a list
--  of (node, location) pairs. This function creates undirected graphs, so you
--  don't need to include reverse links in the adjacency list (though you can
--  if you like).
mkGraphMap :: (Ord a) => [(a,[(a,Cost)])] -> [(a,Location)] -> GraphMap a
mkGraphMap conn loc = G (G.toUndirectedGraph conn) (M.fromList loc)

-- |Get the neighbours of a node from a GraphMap.
getNeighbours :: Ord a => a -> GraphMap a -> [(a,Cost)]
getNeighbours a (G g _) = G.getNeighbours a g

-- |Get the location of a node from a GraphMap.
getLocation :: Ord a => a -> GraphMap a -> Location
getLocation a (G _ l) = case M.lookup a l of
    Nothing -> error "Vertex not found in graph -- GETLOCATION"
    Just pt -> pt

-- | Add an edge between two nodes to a GraphMap.
addEdge :: Ord a => a -> a -> Cost -> GraphMap a -> GraphMap a
addEdge x y cost (G graph locs) = G (G.addUndirectedEdge x y cost graph) locs

-- |The cost associated with moving between two nodes in a GraphMap. If the
--  nodes are not connected by an edge, then the cost is returned as infinity.
costFromTo :: Ord a => GraphMap a -> a -> a -> Cost
costFromTo graph a b = case lookup b (getNeighbours a graph) of
    Nothing -> 1/0
    Just c  -> c

-- |Data structure to hold a graph problem (represented as a GraphMap together
--  with an initial and final node).
data GraphProblem s a = GP
    { graphGP :: GraphMap s
    , initGP :: s
    , goalGP :: s } deriving (Show,Read)

-- |GraphProblems are an instance of Problem. The heuristic function measures
--  the Euclidean (straight-line) distance between two nodes. It is assumed that
--  this is less than or equal to the cost of moving along edges.
instance Ord s => Problem GraphProblem s s where
    initial = initGP
    goal = goalGP
    successor (GP g _ _) s = [ (x,x) | (x,_) <- getNeighbours s g ]
    costP (GP g _ _) c s _ s' = c + costFromTo g s s'
    heuristic (GP g _ goal) n = euclideanDist x y
        where
            x = getLocation (state n) g
            y = getLocation goal g

-- |Measures the Euclidean (straight-line) distance between two locations.
euclideanDist :: Location -> Location -> Double
euclideanDist (x,y) (x',y') = sqrt $ (x-x')^2 + (y-y')^2
