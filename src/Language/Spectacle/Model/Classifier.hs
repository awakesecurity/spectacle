{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Model.Classifier
  ( -- * Tabula State
    Tabula (Tabula),
    getTabula,
    propBinder,

    -- ** Lenses
    ixDepth,
    ixWorld,
    ixSeen,
    propBinderIdx,

    -- ** Query
    ixTruth,
    ixAction,

    -- * Tabula Nodes
    TabulaNode (TabulaNode),
    nodeNexts,
    propTruth,

    -- ** Lenses
    nextNodes,
    nodeTruths,
    ixMaybeProp,
    ixProp,
  )
where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro (Lens', SimpleGetter, at, ix, lens, to, (<&>), (^.))
import Lens.Micro.Internal (At, Index, IxValue, Ixed)

import Data.World (World (World))
import Language.Spectacle.Checker.Fingerprint (Fingerprint)

-- ---------------------------------------------------------------------------------------------------------------------

data Tabula = Tabula
  { getTabula :: IntMap TabulaNode
  , propBinder :: Map String Int
  , actBinder :: Map String Int
  , searchDepth :: {-# UNPACK #-} !Int
  }
  deriving (Show)

-- | @since 0.1.0.0
type instance Index Tabula = Fingerprint

-- | @since 0.1.0.0
type instance IxValue Tabula = TabulaNode

-- | @since 0.1.0.0
instance Ixed Tabula where
  ix fp f Tabula {..} =
    let fpInt = fromIntegral fp
        tabula = case IntMap.lookup fpInt getTabula of
          Nothing -> pure getTabula
          Just node -> f node <&> \node' -> IntMap.insertWith (<>) fpInt node' getTabula
     in fmap (\tabula' -> Tabula {getTabula = tabula', ..}) tabula

-- | @since 0.1.0.0
instance At Tabula where
  at k =
    let fpInt = fromIntegral k
        getter = IntMap.lookup fpInt . getTabula
        setter Tabula {..} = \case
          Nothing -> Tabula {..}
          Just node -> Tabula {getTabula = IntMap.insertWith (<>) fpInt node getTabula, ..}
     in lens getter setter

-- | @since 0.1.0.0
instance Semigroup Tabula where
  Tabula tab1 ps1 as1 d1 <> Tabula tab2 ps2 as2 d2 =
    Tabula
      (IntMap.unionWith (<>) tab1 tab2)
      (Map.union ps1 ps2)
      (Map.union as1 as2)
      (d1 `max` d2)
  {-# INLINE (<>) #-}

ixWorld :: World ctxt -> Lens' Tabula (Maybe TabulaNode)
ixWorld (World fp _) = at fp
{-# INLINE ixWorld #-}

ixDepth :: Lens' Tabula Int
ixDepth = lens searchDepth \Tabula {..} d ->
  Tabula {searchDepth = max d searchDepth, ..}
{-# INLINE ixDepth #-}

ixSeen :: World ctxt -> SimpleGetter Tabula Bool
ixSeen (World fp _) = to (IntMap.member (fromIntegral fp) . getTabula)

propBinderIdx :: String -> SimpleGetter Tabula (Maybe Int)
propBinderIdx prop = to (Map.lookup prop . propBinder)
{-# INLINE propBinderIdx #-}

ixTruth :: String -> World ctxt -> Tabula -> Maybe Bool
ixTruth prop world tabula = do
  propIx <- tabula ^. propBinderIdx prop
  node <- tabula ^. ixWorld world
  pure (IntMap.member propIx (propTruth node))

ixAction :: String -> Tabula -> Int
ixAction prop tabula = actBinder tabula Map.! prop

data TabulaNode = TabulaNode
  { nextNodes :: Set Fingerprint
  , propTruth :: IntMap Bool
  }
  deriving (Show)

-- | @since 0.1.0.0
instance Semigroup TabulaNode where
  TabulaNode ns1 ts1 <> TabulaNode ns2 ts2 =
    TabulaNode (Set.union ns1 ns2) (IntMap.unionWith (||) ts1 ts2)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid TabulaNode where
  mempty = TabulaNode Set.empty IntMap.empty
  {-# INLINE mempty #-}

nodeNexts :: Lens' TabulaNode (Set Fingerprint)
nodeNexts = lens nextNodes \TabulaNode {..} x ->
  TabulaNode {nextNodes = Set.union x nextNodes, ..}
{-# INLINE nodeNexts #-}

nodeTruths :: Lens' TabulaNode (IntMap Bool)
nodeTruths = lens propTruth \TabulaNode {..} x ->
  TabulaNode {propTruth = IntMap.unionWith (||) x propTruth, ..}
{-# INLINE nodeTruths #-}

ixMaybeProp :: Int -> Lens' TabulaNode (Maybe Bool)
ixMaybeProp idx = lens (IntMap.lookup idx . propTruth) \TabulaNode {..} x ->
  TabulaNode {propTruth = IntMap.insertWith (||) idx (fromMaybe False x) propTruth, ..}
{-# INLINE ixMaybeProp #-}

ixProp :: Int -> Lens' TabulaNode Bool
ixProp idx = lens (fromMaybe False . IntMap.lookup idx . propTruth) \TabulaNode {..} x ->
  TabulaNode {propTruth = IntMap.insertWith (||) idx x propTruth, ..}
{-# INLINE ixProp #-}
