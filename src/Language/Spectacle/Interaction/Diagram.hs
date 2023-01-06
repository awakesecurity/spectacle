{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Language.Spectacle.Interaction.Diagram
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
-- 
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.Interaction.Diagram where

import Control.Applicative (liftA2)
import Control.Arrow (Kleisli (Kleisli))
import Control.Comonad (Comonad (duplicate))
import Control.Comonad.Store (extract)
import Data.Foldable (Foldable (fold))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe ()
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import Prettyprinter (Doc, pretty)
import qualified Prettyprinter as Doc
import qualified Prettyprinter.Internal as Doc.Internal
import Prettyprinter.Render.Terminal (AnsiStyle, Color (Blue, Cyan, Green, Magenta, Red, Yellow))
import qualified Prettyprinter.Render.Terminal as Doc

import Control.Comonad.Tape (Tape (Tape), after, before, focus, tabulatel, tabulater, viewl)
import Data.Fingerprint (Fingerprint)
import Data.Traversable (for)
import Language.Spectacle.Interaction.Doc
  ( Cardinal (CLeft, CRight),
    cline,
    hline,
    tab,
    tack,
    turnLeftUp,
    turnRightUp,
    turnUpLeft,
    turnUpRight,
  )
import qualified Language.Spectacle.Interaction.Doc as Doc
import Language.Spectacle.Interaction.Paths (takeMinRow)
import Language.Spectacle.Interaction.Point (Point, column, extent, fields, label, parent)

-- ---------------------------------------------------------------------------------------------------------------------

data LogNode = LogNode
  { nodeColor :: Color
  , nodeLabel :: Fingerprint
  }

-- | @'traverseRowsOf' f ps@ is a traversal of @f@ on rows of @ps@, collecting the results in a list.
--
-- @since 1.0.0
traverseRowsOf :: (Set Point -> DiagramM a) -> Set Point -> DiagramM [a]
traverseRowsOf f ps =
  case takeMinRow ps of
    (next, paths')
      | Set.null paths' -> pure @[] <$> f next
      | otherwise -> liftA2 (:) (f next) (traverseRowsOf f paths')

diagramFull :: Set Point -> DiagramM (Doc AnsiStyle)
diagramFull points = do
  rowDocs <- traverseRowsOf diagramRow points
  pure (Doc.concatWith (flip mappend) rowDocs)

diagramRow :: Set Point -> DiagramM (Doc AnsiStyle)
diagramRow rows =
  case viewl (foldMap Seq.singleton rows) of
    Nothing -> pure mempty
    Just points -> do
      branchDoc <- branchSection points
      subtreeDoc <- subtreeDiagram points
      curveDocs <- curveSection points
      return (branchDoc <> Doc.line <> curveDocs <> subtreeDoc)

subtreeDiagram :: Tape Point -> DiagramM (Doc AnsiStyle)
subtreeDiagram = fmap (foldMap (`mappend` Doc.line)) . traverse pointSection . duplicate

pointSection :: Tape Point -> DiagramM (Doc AnsiStyle)
pointSection ps = do
  lx <- labelSection ps
  fs <- fieldSection ps
  pure (lx <> Doc.vsep fs)

-- | @'labelSection' points@ documents the label of the focused points as well as neighboring paths, for example:
--
-- @
-- | * | 0x8f46a202
-- @
--
-- @since 1.0.0
labelSection :: Tape Point -> DiagramM (Doc AnsiStyle)
labelSection ps = do
  paths <- labelPaths ps
  let lx = Doc.annotate (Doc.colorDull Yellow) (pretty (focus ps ^. label))
  pure (paths <> lx <> Doc.line)

-- | @'fieldSection' points@ documents each of the fields in the focused point as well a neighboring paths, for example:
--
-- @
-- | | |   #field1 = "foo"
-- | | |   #field2 = 12345
-- @
--
-- @since 1.0.0
fieldSection :: Tape Point -> DiagramM [Doc AnsiStyle]
fieldSection points = do
  paths <- fieldPaths points
  let fs = extract points ^. fields
  pure (map (mappend paths . Doc.indent 2) fs)

branchSection :: Tape Point -> DiagramM (Doc AnsiStyle)
branchSection = foldr cons (pure mempty) . duplicate
  where
    cons points
      | focus points ^. extent == 0 = id
      | otherwise = liftA2 (<>) (go points)

    go points@Tape {..} = do
      doc <- branchSegment points lcol ucol
      withColor doc (focus ^. label)
      where
        lcol = sum (view extent <$> before)
        ucol = lcol + focus ^. extent

curveSection :: Tape Point -> DiagramM (Doc AnsiStyle)
curveSection = foldr cons (pure mempty) . duplicate
  where
    cons points
      | focus points ^. extent == 0 = id
      | otherwise = liftA2 mappend (go points)

    go points@Tape {..}
      | lcol <= col && col < ucol = pure mempty
      | otherwise = do
        curve <- curveSegment points lcol ucol
        withColor (curve <> Doc.line) (focus ^. label)
      where
        lcol = sum (view extent <$> before)
        ucol = lcol + focus ^. extent
        col = focus ^. column

curveSegment :: Tape Point -> Int -> Int -> DiagramM (Doc AnsiStyle)
curveSegment points lcol ucol
  | col < lcol = do
    let curve = rightTurnSegment (lcol - col) <> turnRightUp
    ls <- for (tabulatel points) \ps -> do
      if focus ps ^. extent == 0
        then pure tab
        else withColor (verticalSegment 1) (focus ps ^. label)

    withColor (fold ls <> curve) lx
  | ucol < 1 + col = do
    let curve = turnLeftUp <> Doc.copies (2 * (col - ucol) + 1) hline <> turnUpLeft
    rs <- for (tabulater points) \ps -> do
      if focus ps ^. extent == 0
        then pure tab
        else withColor (verticalSegment 1) (focus ps ^. label)
    ls <- for (tabulatel points) \ps -> do
      if focus ps ^. extent == 0
        then pure mempty
        else do
          let spc = Doc.tabs (focus ps ^. extent - 1)
          doc <- withColor (verticalSegment 1) (focus ps ^. label)
          pure (spc <> doc)

    let spc = Doc.tabs (focus points ^. extent - 1)
    withColor (fold ls <> spc <> curve <> Doc.space <> fold rs) lx
  | otherwise = pure mempty
  where
    col = focus points ^. column
    lx = focus points ^. label

-- | @'labelPaths' points@ inserts a line segment for each point in @points@, and a asterrisk for the point under
-- focus.
--
-- @since 1.0.0
labelPaths :: Tape Point -> DiagramM (Doc AnsiStyle)
labelPaths = iteratePaths (const (pure "* "))

-- | @'labelPaths' points@ inserts a line segment for each point in @points@.
--
-- @since 1.0.0
fieldPaths :: Tape Point -> DiagramM (Doc AnsiStyle)
fieldPaths = iteratePaths foci
  where
    foci point = case point ^. parent of
      Nothing -> pure tab
      Just lx -> withColor (verticalSegment 1) lx

branchSegment :: Tape Point -> Int -> Int -> DiagramM (Doc AnsiStyle)
branchSegment points lcol ucol
  | col <= lcol = pure $ rightFanoutSegment len
  | ucol <= 1 + col = pure $ leftFanoutSegment len
  | otherwise = do
    let ls = leavesSegment (col - lcol - 1)
    let rs = leavesSegment (ucol - col - 2)
    let fans = Doc.cat [turnLeftUp, hline, ls, cline, Doc.hline, rs, turnRightUp]
    pure (fans <> Doc.space)
  where
    col = focus points ^. column
    len = focus points ^. extent

iteratePaths :: (Point -> DiagramM (Doc AnsiStyle)) -> Tape Point -> DiagramM (Doc AnsiStyle)
iteratePaths f = iterateLine groupBefore f groupAfter

groupAfter :: Point -> DiagramM (Doc AnsiStyle)
groupAfter point = do
  let len = min (point ^. extent) 1
  let lx = point ^. label
  withColor (verticalSegment len) lx

groupBefore :: Point -> DiagramM (Doc AnsiStyle)
groupBefore point =
  case point ^. parent of
    Nothing -> pure Doc.tab
    Just lx -> withColor (verticalSegment 1) lx

iterateLine ::
  (Point -> DiagramM (Doc AnsiStyle)) ->
  (Point -> DiagramM (Doc AnsiStyle)) ->
  (Point -> DiagramM (Doc AnsiStyle)) ->
  Tape Point ->
  DiagramM (Doc AnsiStyle)
iterateLine handleLT handleEQ handleGT points = do
  lt <- foldr (liftA2 (<>) . handleLT) (pure mempty) (before points)
  gt <- foldr (liftA2 (<>) . handleGT) (pure mempty) (after points)
  eq <- handleEQ (focus points)
  pure (lt <> eq <> gt)

-- | @'verticalSegment' len@ fills a line of length @len@ comprised of segments "│ ".
--
-- @since 1.0.0
verticalSegment :: Int -> Doc AnsiStyle
verticalSegment len
  | len <= 0 = Doc.tab
  | len == 1 = Doc.Internal.Text 2 "│ "
  | otherwise = Doc.copies len (Doc.Internal.Text 2 "│ ")

leftFanoutSegment :: Int -> Doc AnsiStyle
leftFanoutSegment len
  | len <= 0 = mempty
  | len == 1 = Doc.Internal.Text 2 "│ "
  | otherwise =
    let begin = Doc.Internal.Text 2 "╰─"
        end = tack CLeft <> Doc.space
     in begin <> leavesSegment (len - 2) <> end

rightFanoutSegment :: Int -> Doc AnsiStyle
rightFanoutSegment len
  | len <= 0 = mempty
  | len == 1 = Doc.Internal.Text 2 "│ "
  | otherwise =
    let begin = tack CRight <> Doc.hline
        end = Doc.Internal.Text 2 "╯ "
     in begin <> leavesSegment (len - 2) <> end

rightTurnSegment :: Int -> Doc AnsiStyle
rightTurnSegment len
  | len <= 0 = mempty
  | len == 1 = Doc.Internal.Text 2 "╭─"
  | otherwise = turnUpRight <> Doc.copies (2 * len - 1) (Doc.Internal.Char '─')

-- | @'leavesPaths' len@ fills a line of length @len@ comprised of segments "┴─".
--
-- @since 1.0.0
leavesSegment :: Int -> Doc AnsiStyle
leavesSegment len
  | len <= 0 = mempty
  | len == 1 = Doc.Internal.Text 2 "┴─"
  | otherwise = Doc.copies len (Doc.Internal.Text 2 "┴─")

withColor :: Doc AnsiStyle -> Fingerprint -> DiagramM (Doc AnsiStyle)
withColor doc hash = fmap (\c -> Doc.annotate (Doc.color c) doc) (colorOf hash)

-- ---------------------------------------------------------------------------------------------------------------------

newtype DiagramM a = DiagramM
  {runDiagramM :: IORef DiagramCtx -> IO a}
  deriving
    (Functor, Applicative, Monad)
    via Kleisli IO (IORef DiagramCtx)

runDiagram :: DiagramM a -> IO a
runDiagram (DiagramM io) = newIORef (DiagramCtx (ColorSrc mempty mempty)) >>= io

newtype DiagramCtx = DiagramCtx {ctxColorSource :: ColorSrc}

colorOf :: Fingerprint -> DiagramM Color
colorOf hash =
  DiagramM \ref -> do
    ctx <- readIORef ref
    case lookupColor hash (ctxColorSource ctx) of
      Nothing -> do
        let (c, cs') = takeColor hash (ctxColorSource ctx)
        writeIORef ref ctx {ctxColorSource = cs'}
        pure c
      Just c -> pure c

-- ---------------------------------------------------------------------------------------------------------------------

data ColorSrc = ColorSrc
  { srcColorCtx :: {-# UNPACK #-} !ColorCtx
  , srcColorMap :: !(IntMap Color)
  }

lookupColor :: Fingerprint -> ColorSrc -> Maybe Color
lookupColor hash (ColorSrc _ cs) = IntMap.lookup (fromIntegral hash) cs

takeColor :: Fingerprint -> ColorSrc -> (Color, ColorSrc)
takeColor hash (ColorSrc !cctx cmap) =
  let c = getColorCtx cctx
      cctx' = succ cctx
      cmap' = IntMap.insert (fromIntegral hash) c cmap
   in (c, ColorSrc cctx' cmap')

-- ---------------------------------------------------------------------------------------------------------------------

newtype ColorCtx = ColorCtx Int
  deriving (Eq, Ord)

getColorCtx :: ColorCtx -> Color
getColorCtx (ColorCtx i) =
  case mod i 6 of
    0 -> Cyan
    1 -> Red
    2 -> Green
    3 -> Yellow
    4 -> Blue
    _ -> Magenta
{-# INLINE getColorCtx #-}

-- | @since 1.0.0
instance Semigroup ColorCtx where
  -- Note: We use addition in Z mod 6 as the semigroup for 'ColorCtx' since the 'Enum' instance is cycles through 6
  -- colors.

  ColorCtx x <> ColorCtx y = ColorCtx (mod (x + y) 6)
  {-# INLINE (<>) #-}

-- | @since 1.0.0
instance Monoid ColorCtx where
  mempty = ColorCtx 0
  {-# INLINE mempty #-}

-- | @since 1.0.0
instance Enum ColorCtx where
  -- Note: AnsiStyle have six unique colors that are not black or white (cyan, red, green, yellow, blue and magenta).
  -- 'DiagramM' uses this 'Enum' instance and 'succ' to cycle through these six colors since the 'Color' datatype
  -- exported does not provide one out of the box.

  toEnum i = ColorCtx (mod i 6)
  {-# INLINE toEnum #-}

  fromEnum (ColorCtx i) = mod i 6
  {-# INLINE fromEnum #-}

  succ (ColorCtx i) = ColorCtx (mod (succ i) 6)
  {-# INLINE succ #-}
