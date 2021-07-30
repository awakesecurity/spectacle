{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Specifications.SpanningTree where

import Control.Monad (forM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Exception (throwIO)

import Language.Spectacle
  ( Action,
    Initial,
    Invariant,
    Terminate,
    defaultInteraction,
    define,
    enabled,
    exists,
    modelCheck,
    plain,
    strongFair,
    (.=),
    (==>),
    (\/),
    type (#),
  )
import Language.Spectacle.Specification
  ( Specification
      ( Specification,
        fairnessConstraint,
        initialAction,
        nextAction,
        temporalFormula,
        terminationFormula
      ),
  )

-- ---------------------------------------------------------------------------------------------------------------------

data Constants = Constants
  { nodes :: [String]
  , edges :: [(String, String)]
  , root :: String
  , maxCardinality :: Int
  }

data Node = Node
  { nodeId :: String
  , nodeDist :: Int
  , nodeParent :: String
  , nodeNeighbors :: [String]
  }

type SpanningTree =
  '[ -- Parent/Child relationship in the edges of the spanning tree where the key of the hashmap is the child node and
     -- the mapped value is the parent.
     "relations" # HashMap String String
   , -- Mapping from a node in the spanning tree to the distance from its parent.
     "dists" # HashMap String Int
   ]

neighbors :: (?constants :: Constants) => String -> [String]
neighbors node = [m | (m, n) <- edges ?constants, node == n]

initial :: (?constants :: Constants) => Initial SpanningTree ()
initial = do
  let Constants {..} = ?constants
  #relations `define` do
    let mom = [(node, node) | node <- nodes]
    return (HashMap.fromList mom)
  #dists `define` do
    let dists = [(node, maxCardinality) | node <- nodes, node /= root]
    return (HashMap.fromList ((root, 0) : dists))

next :: (?constants :: Constants) => Action SpanningTree Bool
next = do
  let Constants {..} = ?constants

  relations <- plain #relations
  dists <- plain #dists

  exists nodes \n ->
    exists (neighbors n) \m -> do
      let distM :: Int = distance m dists
          distN :: Int = distance n dists
      exists [distM + 1 .. distN - 1] \d -> do
        #dists .= return (HashMap.insert n d dists)
        #relations .= return (HashMap.insert n m relations)
        return (distM < 1 + distN && m /= n)
  where
    distance :: (?constants :: Constants) => String -> HashMap String Int -> Int
    distance n ds = HashMap.lookupDefault (maxCardinality ?constants) n ds

formula :: (?constants :: Constants) => Invariant SpanningTree Bool
formula = do
  not <$> enabled ==> postCondition
  where
    postCondition :: Invariant SpanningTree Bool
    postCondition = do
      let Constants {..} = ?constants
      and <$> forM nodes \node ->
        isRoot node \/ isLeaf node \/ isNode node

getNode :: (?constants :: Constants) => String -> Invariant SpanningTree (Maybe Node)
getNode name = do
  dist <- HashMap.lookup name <$> plain #dists
  parent <- HashMap.lookup name <$> plain #relations
  return (Node name <$> dist <*> parent <*> pure (neighbors name))

furthestNeighbors :: (?constants :: Constants) => String -> Invariant SpanningTree Bool
furthestNeighbors name = do
  let Constants {..} = ?constants
  getNode name >>= \case
    Nothing -> return False
    Just Node {..} -> do
      dists <- plain #dists
      return (all (\n -> Just maxCardinality == HashMap.lookup n dists) nodeNeighbors)

isRoot :: (?constants :: Constants) => String -> Invariant SpanningTree Bool
isRoot name = do
  let Constants {..} = ?constants
  getNode name >>= \case
    Nothing -> return False
    Just Node {..} -> do
      furthest <- furthestNeighbors name
      return (nodeId == root && nodeDist == 0 && furthest)

isLeaf :: (?constants :: Constants) => String -> Invariant SpanningTree Bool
isLeaf name = do
  let Constants {..} = ?constants
  getNode name >>= \case
    Nothing -> return False
    Just Node {..} -> do
      furthest <- furthestNeighbors name
      return (nodeDist == maxCardinality && name == nodeParent && furthest)

isNode :: (?constants :: Constants) => String -> Invariant SpanningTree Bool
isNode name = do
  let Constants {..} = ?constants
  getNode name >>= \case
    Nothing -> return False
    Just Node {..} -> do
      dists <- plain #dists
      let closeToParent = maybe False (\d -> nodeDist == d + 1) (HashMap.lookup nodeParent dists)
      return (1 <= nodeDist && nodeDist <= maxCardinality && nodeParent `elem` nodeNeighbors && closeToParent)

termination :: Terminate ctx Bool
termination = not <$> enabled

check :: IO ()
check = do
  -- let ?constants =
  --       Constants
  --         { nodes = ["a", "b", "c", "d", "e"]
  --         , edges = [("a", "b"), ("a", "d"), ("b", "c"), ("d", "c"), ("c", "e"), ("e", "d")]
  --         , root = "a"
  --         , maxCardinality = 4
  --         }
  let ?constants =
        Constants
          { nodes = ["a", "b", "c"]
          , edges = [("a", "b"), ("b", "c")]
          , root = "a"
          , maxCardinality = 2
          }
  let spec :: Specification SpanningTree
      spec =
        Specification
          { initialAction = initial
          , nextAction = next
          , temporalFormula = formula
          , terminationFormula = Just termination
          , fairnessConstraint = strongFair
          }
  defaultInteraction (modelCheck spec)
