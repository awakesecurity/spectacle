{-# LANGUAGE RecordWildCards #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Model.ModelState
  ( -- * ModelState
    ModelState (ModelState),
    getModelState,

    -- ** Insertion
    record,

    -- ** Update
    update,

    -- ** Query
    query,
    member,
    notMember,

    -- ** Lenses
    indexOf,

    -- * Checker State Nodes
    ModelNode (ModelNode),

    -- ** Lenses
    nodeEntries,
    nodeActions,
    entryOf,

    -- * Node Entries
    ModelEntry (ModelEntry),

    -- ** Construction
    newModelEntry,

    -- ** Lenses
    enabled
  )
where

import Data.Coerce (coerce)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro (Lens', lens)

import Data.Fingerprint (Fingerprint (Fingerprint))

-- ---------------------------------------------------------------------------------------------------------------------

newtype ModelState = ModelState
  {getModelState :: IntMap ModelNode}
  deriving (Show)
  deriving (Semigroup, Monoid) via IntMap ModelNode

-- | @'record' hash node st@ inserts the 'ModelNode' @node@ at the key @hash@, or unions node at @hash@ if one already
-- exists at the position.
--
-- @since 0.1.0.0
record :: Fingerprint -> ModelNode -> ModelState -> ModelState
record hash node = coerce . IntMap.insertWith (<>) (coerce hash) node . coerce

update :: Fingerprint -> (ModelNode -> ModelNode) -> ModelState -> ModelState
update hash f =
  let mkalteration = \case
        Nothing -> f mempty
        Just node -> f node <> node
   in coerce . IntMap.alter (Just . mkalteration) (coerce hash) . coerce

query :: Fingerprint -> ModelState -> ModelNode
query hash = IntMap.findWithDefault mempty (coerce hash) . coerce

-- | Is there a 'ModelNode' mapped to the given 'Fingerprint'?
--
-- @since 0.1.0.0
member :: Fingerprint -> ModelState -> Bool
member hash = IntMap.member (coerce hash) . getModelState

-- | Is there no 'ModelNode' mapped to the given 'Fingerprint'?
--
-- @since 0.1.0.0
notMember :: Fingerprint -> ModelState -> Bool
notMember hash = IntMap.notMember (coerce hash) . getModelState

indexOf :: Fingerprint -> Lens' ModelState ModelNode
indexOf hash = lens (query hash) (flip (record hash))

-- ---------------------------------------------------------------------------------------------------------------------

data ModelNode = ModelNode
  { modelNodeEntries :: !(Map String ModelEntry)
  , modelNodeActions :: !(Set String)
  }
  deriving (Show)

-- | @since 0.1.0.0
instance Semigroup ModelNode where
  ModelNode ens0 acts0 <> ModelNode ens1 acts1 =
    let entries = Map.unionWith (<>) ens0 ens1
        actions = acts0 <> acts1
     in ModelNode entries actions
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid ModelNode where
  mempty = ModelNode Map.empty Set.empty
  {-# INLINE CONLIKE mempty #-}

nodeEntries :: Lens' ModelNode (Map String ModelEntry)
nodeEntries =
  let getter ModelNode {..} ens =
        ModelNode {modelNodeEntries = Map.unionWith (<>) modelNodeEntries ens, ..}
   in lens modelNodeEntries getter

nodeActions :: Lens' ModelNode (Set String)
nodeActions =
  let getter ModelNode {..} acts =
        ModelNode {modelNodeActions = modelNodeActions <> acts, ..}
   in lens modelNodeActions getter

entryOf :: String -> Lens' ModelNode ModelEntry
entryOf name =
  let getter = fromMaybe mempty . Map.lookup name . modelNodeEntries
      setter ~(ModelNode xs as) en = ModelNode (Map.insertWith (mappend @ModelEntry) name en xs) as
   in lens getter setter

-- ---------------------------------------------------------------------------------------------------------------------

data ModelEntry = ModelEntry
  { entryIsEnabled :: !Bool
  , entryEntails :: !(Set Fingerprint)
  }
  deriving (Show)

-- | Constructs a 'ModelEntry' from a set of fingerprints, flagged as enabled when the given set is non-empty.
--
-- @since 0.1.0.0
newModelEntry :: Set Fingerprint -> ModelEntry
newModelEntry hs
  | Set.null hs = ModelEntry False hs
  | otherwise = ModelEntry True hs

-- | @since 0.1.0.0
instance Semigroup ModelEntry where
  ModelEntry en0 xs <> ModelEntry en1 ys = ModelEntry (en0 || en1) (xs <> ys)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid ModelEntry where
  mempty = ModelEntry False Set.empty
  {-# INLINE CONLIKE mempty #-}

enabled :: Lens' ModelEntry Bool
enabled =
  let setter ~(ModelEntry en im) x = ModelEntry (en || x) im
   in lens entryIsEnabled setter

-- entailed :: Lens' ModelEntry (Set Fingerprint)
-- entailed =
--   let getter (ModelEntry p) = snd p
--       setter ~(ModelEntry p) xs = ModelEntry (fst p, xs)
--    in lens getter setter
