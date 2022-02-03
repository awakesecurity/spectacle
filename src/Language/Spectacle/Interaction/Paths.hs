{-# LANGUAGE RecordWildCards #-}

-- |
--
-- @since 1.0.0
module Language.Spectacle.Interaction.Paths
  ( -- * Construction
    toPointSet,
    takeMinRow,
  )
where

import qualified Data.Set as Set
import Data.Set.Internal (Set (Bin, Tip))
import Lens.Micro ((.~), (?~), (^.))

import Control.Monad.State (execState, gets, modify)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor.Tree (Tree, pattern (:-))
import Data.Type.Rec (HasDict)
import Data.World (World, fingerprint)
import Language.Spectacle.Interaction.Point (Point, column, extent, fromWorld, parent, row)

-- ---------------------------------------------------------------------------------------------------------------------

toPointSet :: HasDict Show ctx => Tree (World ctx) -> Set Point
toPointSet = flip execState Set.empty . start
  where
    start (w :- ws) = do
      c <- gets (columnsOn 0)
      let point =
            fromWorld w
              & column .~ c
              & extent .~ length ws

      modify (Set.insert point)
      traverse_ (go 1 (w ^. fingerprint)) ws

    go row0 par (w :- ws) = do
      col <- gets (columnsOn row0)
      let point =
            fromWorld w
              & parent ?~ par
              & extent .~ length ws
              & column .~ col
              & row .~ row0
      modify (Set.insert point)
      traverse_ (go (1 + row0) (w ^. fingerprint)) ws

-- | Like 'splitRow', but always splits on the least row in the set.
--
-- @since 1.0.0
takeMinRow :: Set Point -> (Set Point, Set Point)
takeMinRow ps =
  case Set.minView ps of
    Nothing -> (Set.empty, Set.empty)
    Just (p, _) -> splitRow (p ^. row) ps

-- | @'splitRow' i ps@ returns a subset of @ps@ containing all elements located on row @i@ and a new set stripped of
-- this row.
--
-- @since 1.0.0
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
columnsOn i = Set.size . Set.filter (\pt -> pt ^. row == i)
