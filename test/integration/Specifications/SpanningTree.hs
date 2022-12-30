{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -fno-warn-orphans #-}

module Specifications.SpanningTree where

import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Traversable (for)
import GHC.Exts (IsString)
import Language.Spectacle
  ( Action,
    ActionType (ActionWF),
    Fairness (WeakFair),
    Modality (Always),
    Specification (Specification),
    Temporal,
    TemporalType (PropG),
    exists,
    interaction,
    plain,
    prime,
    specInit,
    specNext,
    specProp,
    (.=),
    pattern ConF,
    pattern NilF,
    type (#),
  )

-- ---------------------------------------------------------------------------------------------------------------------

interact :: IO ()
interact = interaction spanTreeSpec

-- ---------------------------------------------------------------------------------------------------------------------

data Constants = Constants
  { constNodes :: [Node]
  , constEdges :: [(Node, Node)]
  , constRoot :: Node
  }

newtype Node = Node {getNode :: String}
  deriving (Eq, Ord, Hashable, IsString, Show) via String

-- ---------------------------------------------------------------------------------------------------------------------

type SpanTreeSpec = Specification SpecVar SpecActions SpecProp

type SpecVar =
  '[ "hierarchy" # Map Node Node
   , "distances" # Map Node Int
   ]

type SpecActions =
  '[ "nextSpan" # 'WeakFair
   ]

type SpecProp =
  '[ "safety" # 'Always
   ]

spanTreeSpec :: SpanTreeSpec
spanTreeSpec =
  let specInit =
        ConF #hierarchy (pure initHierarchy)
          . ConF #distances (pure initDistances)
          $ NilF
      specNext =
        let ?constants = spanTreeConstants
         in ConF #nextSpan (ActionWF nextSpan) NilF
      specProp =
        let ?constants = spanTreeConstants
         in ConF #safety (PropG safety) NilF
   in Specification {..}
  where
    constants = spanTreeConstants

    maxCardinality = length (constNodes constants)

    initHierarchy = Map.fromList (map (\n -> (n, n)) (constNodes constants))

    initDistances = Map.fromList do
      node <- constNodes constants
      if node == constRoot constants
        then pure (node, 0)
        else pure (node, maxCardinality)

spanTreeConstants :: Constants
spanTreeConstants =
  let constNodes = ["a", "b", "c"]
      constEdges = [("a", "b"), ("b", "c")]
      constRoot = "a"
   in Constants {..}

nextSpan :: (?constants :: Constants) => Action SpecVar Bool
nextSpan = do
  hierarchy <- plain #hierarchy
  distances <- plain #distances
  exists constNodes \n ->
    exists (neighborsOf n) \m -> do
      let distM = distances Map.! m
      let distN = distances Map.! n
      exists [distM + 1 .. distN - 1] \d -> do
        #distances .= pure (Map.insert n d distances)
        #hierarchy .= pure (Map.insert n m hierarchy)
        return (distM < 1 + distN && m /= n)
  where
    Constants {..} = ?constants

safety :: (?constants :: Constants) => Temporal SpecVar Bool
safety = do
  distances <- plain #distances
  distances' <- prime #distances
  if distances == distances'
    then
      and <$> for constNodes \node -> do
        root <- isRoot node
        leaf <- isLeaf node
        branch <- isBranch node
        pure (root || leaf || branch)
    else pure True
  where
    Constants {..} = ?constants

    maxCardinality = length constNodes

    isLeaf node = do
      distance <- fmap (Map.! node) (plain #distances)
      neighbor <- fmap (Map.! node) (plain #hierarchy)

      ns <- for (neighborsOf node) \nbr -> do
        d <- fmap (Map.! nbr) (plain #distances)
        pure (d == maxCardinality)

      pure (and ns && neighbor == node && distance == maxCardinality)

    isBranch node = do
      distance <- fmap (Map.! node) (plain #distances)
      neighbor <- fmap (Map.! node) (plain #hierarchy)
      nbrDist <- fmap (Map.! neighbor) (plain #distances)

      pure ((neighbor `elem` neighborsOf node) && distance == nbrDist + 1)

    isRoot node = do
      distance <- fmap (Map.! node) (plain #distances)
      neighbor <- fmap (Map.! node) (plain #hierarchy)
      pure (node == constRoot && distance == 0 && neighbor == node)

neighborsOf :: (?constants :: Constants) => Node -> [Node]
neighborsOf node = [m | (m, n) <- constEdges, node == n]
  where
    Constants {..} = ?constants
