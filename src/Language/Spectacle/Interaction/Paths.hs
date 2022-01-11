{-# LANGUAGE RecordWildCards #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Interaction.Paths
  ( -- * Construction
    flatten,
    takeMinRow,
  )
where

import Control.Monad (filterM)
import Control.Monad.Trans.State.Strict (execState, gets, modify)
import Data.Foldable (traverse_)
import qualified Data.Set as Set
import Data.Set.Internal (Set (Bin, Tip))
import Lens.Micro ((&), (.~), (?~), (^.))
import Lens.Micro.Extras (view)

import Data.Functor.Tree (Tree, rootOf, pattern (:-))
import Data.Type.Rec (HasDict)
import Data.World (World, worldFingerprint)
import Language.Spectacle.Interaction.Point (Point, column, extent, fromWorld, label, parent, row)

-- ---------------------------------------------------------------------------------------------------------------------

flatten :: HasDict Show ctx => Tree (World ctx) -> Set Point
flatten ts = execState (start ts) Set.empty
  where
    start (w :- ws) = do
      c <- gets (columnsOn 0)
      let point =
            fromWorld w
              & column .~ c
              & extent .~ length ws

      modify (Set.insert point)
      traverse_ (go 1 (w ^. worldFingerprint)) ws

    go row0 par (w :- ws) = do
      col <- gets (columnsOn row0)
      unique <- filterM (\x -> gets (Set.notMember (rootOf x ^. worldFingerprint) . Set.map (view label))) ws
      let point =
            fromWorld w
              & parent ?~ par
              & extent .~ length unique
              & column .~ col
              & row .~ row0

      modify (Set.insert point)
      traverse_ (go (1 + row0) (w ^. worldFingerprint)) unique

-- | Like 'splitRow', but always splits on the least row in the set.
--
-- @since 0.1.0.0
takeMinRow :: Set Point -> (Set Point, Set Point)
takeMinRow ps =
  case Set.minView ps of
    Nothing -> (Set.empty, Set.empty)
    Just (p, _) -> splitRow (p ^. row) ps

-- | @'splitRow' i ps@ returns a subset of @ps@ containing all elements located on row @i@ and a new set stripped of
-- this row.
--
-- @since 0.1.0.0
splitRow :: Int -> Set Point -> (Set Point, Set Point)
splitRow i = seek Set.empty
  where
    seek acc Tip = (acc, Tip)
    seek acc (Bin _ !p ls rs) =
      case compare (p ^. row) i of
        LT ->
          let (acc', rs') = seek acc rs
           in (acc', Set.union (Set.insert p rs') ls)
        GT ->
          let (acc', ls') = seek acc ls
           in (acc', Set.union (Set.insert p ls') rs)
        EQ ->
          let (acc0, ls') = seek acc ls
              (acc1, rs') = seek acc0 rs
           in (Set.insert p acc1, Set.union ls' rs')

columnsOn :: Int -> Set Point -> Int
columnsOn i = go 0
  where
    go c Tip = c
    go c (Bin _ !p ls rs) =
      case compare (p ^. row) i of
        LT -> go c rs
        GT -> go c ls
        EQ -> 1 + go c ls + go c rs