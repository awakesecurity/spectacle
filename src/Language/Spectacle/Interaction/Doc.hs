{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Interaction.Doc
  ( -- * Direction
    Cardinal (CUp, CDown, CLeft, CRight),

    -- * Doc Combinations
    sepBy,
    hcopies,
    copies,
    tab,
    tabs,
    marginr,
    marginl,
    spaces,
    segment,
    segmentWith,
    hruler,

    -- * Line Characters
    vline,
    hline,
    cline,

    -- * Tack Characters
    tack,

    -- * Turn Characters
    turnLeftUp,
    turnRightUp,
    turnUpLeft,
    turnUpRight,
  )
where

import qualified Data.Text as Text
import Prettyprinter (Doc, FusionDepth (Deep))
import qualified Prettyprinter as Doc
import qualified Prettyprinter.Internal as Doc.Internal

-- ---------------------------------------------------------------------------------------------------------------------

data Cardinal
  = CUp
  | CDown
  | CLeft
  | CRight
  deriving (Enum, Eq, Ord, Show)

-- | Concat a foldable collection with a seperator interspersed.
--
-- @since 0.1.0.0
sepBy :: Foldable t => Doc ann -> t (Doc ann) -> Doc ann
sepBy s = Doc.concatWith (Doc.surround s)

-- | Like 'copies', but intersperses space between each copy.
--
-- @since 0.1.0.0
hcopies :: Int -> Doc ann -> Doc ann
hcopies len doc
  | len <= 0 = Doc.Internal.Empty
  | otherwise = Doc.hsep (replicate len doc)

-- | @'copies' n doc@ is a more optimal combinator for @Doc.sep . replicate n doc@
--
-- @since 0.1.0.0
copies :: Int -> Doc ann -> Doc ann
copies len doc
  | len <= 0 = Doc.Internal.Empty
  | otherwise = Doc.cat (replicate len doc)

-- | @'tab'@ inserts a two-space tab.
--
-- @since 0.1.0.0
tab :: Doc ann
tab = Doc.Internal.Text 2 "  "

-- | @'tabs' n@ inserts n-many tabs.
--
-- @since 0.1.0.0
tabs :: Int -> Doc ann
tabs len = spaces (2 * len)

-- | @'spaces' n@ inserts n-many spaces.
--
-- @since 0.1.0.0
spaces :: Int -> Doc ann
spaces len
  | len <= 0 = Doc.Internal.Empty
  | len == 1 = Doc.Internal.Char ' '
  | otherwise = Doc.Internal.Text len (Text.replicate len $ Text.singleton ' ')

-- | @'marginr' x n@ inserts @x@ and n-many spaces after.
--
-- @since 0.1.0.0
marginr :: Int -> Doc ann -> Doc ann
marginr len doc = Doc.fuse Deep (doc <> spaces len)

-- | @'marginr' x n@ inserts @x@ and n-many spaces after.
--
-- @since 0.1.0.0
marginl :: Int -> Doc ann -> Doc ann
marginl len doc = Doc.fuse Deep (spaces len <> doc)

-- | @'segment' n a b@ is a line segment of length @n@ with @a@, @b@ the left and right endpoints.
--
-- @since 0.1.0.0
segment :: Int -> Doc ann -> Doc ann -> Doc ann
segment len endl endr
  | len == 0 = Doc.Internal.Empty
  | len <= 2 = Doc.fuse Deep (endl <> endr)
  | otherwise = Doc.fuse Deep (endl <> hruler (len - 2) <> endr)

-- | Like 'segment', but with an extra argument to specify a 'Doc' used for the line segment.
--
-- @since 0.1.0.0
segmentWith :: Int -> Doc ann -> Doc ann -> Doc ann -> Doc ann
segmentWith len doc endl endr
  | len <= 2 = endl <> endr
  | otherwise = endl <> hcopies len doc <> endr

-- | @'spaces' n@ inserts horizontal ruler of length n.
--
-- @since 0.1.0.0
hruler :: Int -> Doc ann
hruler len
  | len <= 0 = Doc.Internal.Empty
  | len == 1 = Doc.Internal.Char '─'
  | otherwise = Doc.Internal.Text len (Text.replicate len $ Text.singleton '─')

-- ---------------------------------------------------------------------------------------------------------------------

vline :: Doc ann
vline = Doc.Internal.Char '│'

hline :: Doc ann
hline = Doc.Internal.Char '─'

cline :: Doc ann
cline = Doc.Internal.Char '┼'

tack :: Cardinal -> Doc ann
tack = \case
  CUp -> Doc.Internal.Char '┴'
  CDown -> Doc.Internal.Char '┬'
  CLeft -> Doc.Internal.Char '┤'
  CRight -> Doc.Internal.Char '├'

turnUpRight :: Doc ann
turnUpRight = Doc.Internal.Char '╭'

turnRightUp :: Doc ann
turnRightUp = Doc.Internal.Char '╯'

turnUpLeft :: Doc ann
turnUpLeft = Doc.Internal.Char '╮'

turnLeftUp :: Doc ann
turnLeftUp = Doc.Internal.Char '╰'
