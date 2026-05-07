{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Language.Spectacle.Model.Scc
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Strongly-connected-component analysis of the model's state graph plus the
-- fairness predicate that decides whether a given SCC corresponds to a fair
-- behavior. The fair-cycle answer is what enables sound LTL property checking
-- over cyclic state graphs (issues #47 and #56) without the previous
-- @local@-deletion fairness heuristic in @expandAction@, which manufactured
-- spurious terminal stutters at non-terminal states.
--
-- @since 1.0.2
module Language.Spectacle.Model.Scc
  ( -- * SCC summaries
    SccInfo (..),
    computeSccs,

    -- * Reachability
    reachableFrom,
    reachableVia,
    successorsOf,

    -- * Fairness
    isFairScc,
  )
where

import qualified Data.Graph as Graph
import qualified Data.IntMap.Strict as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((^.))

import Data.Fingerprint (Fingerprint (Fingerprint))
import Language.Spectacle.Model.ModelNode (ModelNode, nodeNextEntries)
import Language.Spectacle.Model.ModelState (ModelState (ModelState), getModelState, indexNode)

-- ---------------------------------------------------------------------------------------------------------------------

-- | Pre-computed information about one SCC of the state graph.
data SccInfo = SccInfo
  { -- | The states that make up this SCC.
    sccNodes :: Set Fingerprint
  , -- | Action labels that appear on at least one edge inside the SCC.
    sccActionsTaken :: Set String
  , -- | Action labels that are enabled at /every/ state of the SCC. For
    -- a non-trivial SCC, these are the actions WF requires us to take infinitely
    -- often when the SCC is the lasso of a behavior.
    sccActionsEnabledEverywhere :: Set String
  , -- | Action labels enabled at /some/ state of the SCC. SF acts on these.
    sccActionsEnabledSomewhere :: Set String
  , -- | Whether this SCC has any edge that leaves it. A bottom SCC (BSCC) has
    -- no outgoing edges and is what infinite behaviors eventually trap into.
    sccIsBottom :: Bool
  , -- | A trivial SCC has exactly one state and no self-loop. Such an SCC is
    -- transient (a path passes through and out) unless it is also bottom, in
    -- which case it is a sink: the path stutters there forever.
    sccIsTrivial :: Bool
  }

-- | Compute SCC info for every SCC in the (full) state graph. Callers that only
-- care about reachable behaviors should filter the result with @sccNodes@
-- against 'reachableFrom' on their initial states.
computeSccs :: ModelState ctx -> [SccInfo]
computeSccs mst =
  let raws = Graph.stronglyConnComp (adjacency mst)
      sccSets = map sccToSet raws
      membership :: Map Fingerprint Int
      membership =
        Map.fromList [(h, idx) | (idx, s) <- zip [0 ..] sccSets, h <- Set.toList s]
   in zipWith (mkSccInfo mst membership) [0 ..] sccSets

-- | Reachable states from a set of seed states, following any outgoing edge.
reachableFrom :: ModelState ctx -> Set Fingerprint -> Set Fingerprint
reachableFrom mst = reachableVia mst (\_ _ -> True)

-- | Reachable states from seeds, following only edges @(x, y)@ where
-- @admit x y@ holds. Used by 'Eventually' to walk the @¬P@-edge subgraph.
reachableVia ::
  ModelState ctx ->
  (Fingerprint -> Fingerprint -> Bool) ->
  Set Fingerprint ->
  Set Fingerprint
reachableVia mst admit = go Set.empty
  where
    go seen frontier
      | Set.null frontier = seen
      | otherwise =
          let seen' = Set.union seen frontier
              succs =
                Set.fromList $
                  [ s
                  | h <- Set.toList frontier
                  , s <- successorsOf mst h
                  , admit h s
                  ]
              frontier' = Set.difference succs seen'
           in go seen' frontier'

-- | All next-state fingerprints reachable in one transition from a state.
successorsOf :: ModelState ctx -> Fingerprint -> [Fingerprint]
successorsOf mst h =
  let n = mst ^. indexNode h
   in concat (Map.elems (nodeNextEntries n))

-- | An SCC is fair under given WF/SF action sets when every WF/SF obligation is
-- discharged on the cycle. WF: an action enabled at /every/ state of the cycle
-- must be taken on the cycle. SF: an action enabled at /some/ state of the
-- cycle must be taken on the cycle. Trivial SCCs (size 1, no self-loop) are
-- vacuously fair: paths don't loop on them, so there is no cycle to satisfy.
isFairScc :: Set String -> Set String -> SccInfo -> Bool
isFairScc wfActions sfActions info
  | sccIsTrivial info = True
  | otherwise =
      let taken = sccActionsTaken info
          wfOK =
            all
              (\a -> not (Set.member a (sccActionsEnabledEverywhere info)) || Set.member a taken)
              (Set.toList wfActions)
          sfOK =
            all
              (\a -> not (Set.member a (sccActionsEnabledSomewhere info)) || Set.member a taken)
              (Set.toList sfActions)
       in wfOK && sfOK

-- ---------------------------------------------------------------------------------------------------------------------

-- Build the @stronglyConnComp@-shaped adjacency triples for the full graph.
adjacency :: ModelState ctx -> [(Fingerprint, Fingerprint, [Fingerprint])]
adjacency mst@(ModelState nodes) =
  [ (h, h, successorsOf mst h)
  | h_int <- IntMap.keys nodes
  , let h = Fingerprint h_int
  ]

sccToSet :: Graph.SCC Fingerprint -> Set Fingerprint
sccToSet (Graph.AcyclicSCC h) = Set.singleton h
sccToSet (Graph.CyclicSCC hs) = Set.fromList hs

mkSccInfo :: forall ctx. ModelState ctx -> Map Fingerprint Int -> Int -> Set Fingerprint -> SccInfo
mkSccInfo mst membership idx scc =
  SccInfo
    { sccNodes = scc
    , sccActionsTaken = takenInScc
    , sccActionsEnabledEverywhere = enabledEverywhere
    , sccActionsEnabledSomewhere = enabledSomewhere
    , sccIsBottom = isBottom
    , sccIsTrivial = isTrivial
    }
  where
    inThisScc h = Map.lookup h membership == Just idx

    perNode :: [(Fingerprint, ModelNode ctx)]
    perNode = [(h, mst ^. indexNode h) | h <- Set.toList scc]

    -- Action labels appearing on any (h -> h') edge with both endpoints in this SCC.
    takenInScc :: Set String
    takenInScc =
      Set.fromList
        [ action
        | (_h, n) <- perNode
        , (action, targets) <- Map.toList (nodeNextEntries n)
        , any inThisScc targets
        ]

    -- Action labels enabled at each state in the SCC, intersected/unioned across states.
    enabledAt :: ModelNode ctx -> Set String
    enabledAt n =
      Set.fromList [a | (a, ts) <- Map.toList (nodeNextEntries n), not (null ts)]

    enabledPerNode = [enabledAt n | (_, n) <- perNode]

    enabledEverywhere
      | null enabledPerNode = Set.empty
      | otherwise = foldr1 Set.intersection enabledPerNode

    enabledSomewhere = Set.unions enabledPerNode

    -- Bottom: no edge from this SCC leaves it.
    isBottom =
      not $
        any
          ( \(_h, n) ->
              any (any (not . inThisScc)) (Map.elems (nodeNextEntries n))
          )
          perNode

    -- Trivial: singleton with no self-edge.
    isTrivial = case Set.toList scc of
      [h] -> h `notElem` successorsOf mst h
      _ -> False
