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
    ixWorld,
    propBinderIdx,

    -- ** Query
    ixTruth,

    -- * Tabula Nodes
    TabulaNode (TabulaNode),
    nodeNexts,
    propTruth,

    -- ** Lenses
    nextNodes,
    nodeTruths,
  )
where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
  }

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
  Tabula tab1 ros1 <> Tabula tab2 ros2 =
    Tabula (IntMap.unionWith (<>) tab1 tab2) (Map.union ros1 ros2)
  {-# INLINE (<>) #-}

ixWorld :: World ctxt -> Lens' Tabula (Maybe TabulaNode)
ixWorld (World fp _) = at fp
{-# INLINE ixWorld #-}

propBinderIdx :: String -> SimpleGetter Tabula (Maybe Int)
propBinderIdx prop = to (Map.lookup prop . propBinder)
{-# INLINE propBinderIdx #-}

ixTruth :: String -> World ctxt -> Tabula -> Maybe Bool
ixTruth prop world tabula = do
  propIx <- tabula ^. propBinderIdx prop
  node <- tabula ^. ixWorld world
  pure (IntSet.member propIx (propTruth node))

data TabulaNode = TabulaNode
  { nextNodes :: Set Fingerprint
  , propTruth :: IntSet
  }

-- | @since 0.1.0.0
instance Semigroup TabulaNode where
  TabulaNode ns1 ts1 <> TabulaNode ns2 ts2 =
    TabulaNode (Set.union ns1 ns2) (IntSet.union ts1 ts2)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid TabulaNode where
  mempty = TabulaNode Set.empty IntSet.empty
  {-# INLINE mempty #-}

nodeNexts :: Lens' TabulaNode (Set Fingerprint)
nodeNexts = lens nextNodes \TabulaNode {..} x ->
  TabulaNode {nextNodes = Set.union x nextNodes, ..}
{-# INLINE nodeNexts #-}

nodeTruths :: Lens' TabulaNode IntSet
nodeTruths = lens propTruth \TabulaNode {..} x ->
  TabulaNode {propTruth = IntSet.union x propTruth, ..}
{-# INLINE nodeTruths #-}
