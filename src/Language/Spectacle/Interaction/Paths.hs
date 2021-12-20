{-# LANGUAGE RecordWildCards #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Interaction.Paths
  ( -- * Paths
    Paths,

    -- ** Construction
    flatten,

    -- ** Destruction
    sliceRow,
    splitRow,
    unfoldRow,
    shortestSeq,

    -- ** Query
    columnsOn,
    memberLabel,
  )
where

import Control.Monad.Trans.State.Strict (execState, gets, modify)
import Data.Bifunctor
import Data.Foldable (traverse_)
import Data.Kind (Type)
import qualified Data.Set as Set
import Data.Set.Internal (Set (Bin, Tip))
import Lens.Micro ((^.))
import Lens.Micro.Mtl (view)

import Data.Context (Context)
import Data.Functor.Tree (Tree, pattern (:-))
import Data.World (World)
import Language.Spectacle.Checker.Fingerprint (Fingerprint)
import Language.Spectacle.Interaction.Point (Point (Point), ptlabel, ptpos)
import Language.Spectacle.Interaction.Pos (prow, pattern Pos)

-- ---------------------------------------------------------------------------------------------------------------------

type Paths :: Context -> Type
type Paths ctx = Set (Point ctx)

flatten :: Tree (World ctx) -> Paths ctx
flatten (x :- xs) = execState (start x xs) mempty
  where
    start w ws = do
      let par = Point w Nothing (length ws) (Pos 0 0)
      modify (Set.insert par)
      traverse_ (go par) ws

    go par (w :- ws) = do
      let row = 1 + par ^. ptpos . prow
      col <- gets (columnsOn row)

      let point = Point w (Just par) (length ws) (Pos row col)
      modify (Set.insert point)

      traverse_ (go point) ws

sliceRow :: Int -> Paths ctx -> Set (Point ctx)
sliceRow row = Set.filter \point ->
  view (ptpos . prow) point == row

splitRow :: Paths ctx -> (Set (Point ctx), Paths ctx)
splitRow path =
  case Set.minView path of
    Nothing -> (Set.empty, path)
    Just (pt, points') ->
      let (row, path') = unfoldRow (view (ptpos . prow) pt) points'
       in (Set.insert pt row, path')

unfoldRow :: Int -> Paths ctx -> (Paths ctx, Paths ctx)
unfoldRow row = go Set.empty
  where
    go acc paths =
      case Set.minView paths of
        Nothing -> (acc, paths)
        Just (pt, paths')
          | pt ^. (ptpos . prow) == row -> go (Set.insert pt acc) paths'
          | otherwise -> (acc, Set.insert pt paths')

-- | @'shortestSeq' p xs@ takes smallest subset of @xs@ containing all elements satisfying @p@.
--
-- @since 0.1.0.0
shortestSeq :: Ord a => (a -> Bool) -> Set a -> Set a
shortestSeq match xs0 =
  let xs = Set.toList xs0
      dropsMin = dropWhile (not . match) xs
      dropsMax = dropWhile (not . match) (reverse dropsMin)
   in Set.fromList (reverse dropsMax)

columnsOn :: Int -> Paths ctx -> Int
columnsOn row = Set.size . sliceRow row

memberLabel :: Fingerprint -> Paths ctx -> Bool
memberLabel hash = go
  where
    go Tip = False
    go (Bin _ x ls rs)
      | x ^. ptlabel == hash = True
      | otherwise = go ls || go rs
