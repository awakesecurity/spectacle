{-# LANGUAGE OverloadedStrings #-}

module Language.Spectacle.Interaction.Render
  ( -- * Model Results Docs
    renderModelErrorsDoc,
    renderModelMetrics,

    -- * Error Docs
    renderMCError,
    renderMCInitialErrorDoc,
    renderMCNoInitialStatesErrorDoc,
    renderMCImpasseErrorDoc,
    renderMCActionErrorDoc,
    renderMCStutterErrorDoc,
    renderMCFormulaErrorDoc,
    renderMCFormulaRuntimeErrorDoc,
    errorDoc,

    -- * File Views Docs
    fileHeadingDoc,
    unknownLocDoc,
    formulaLocDoc,
    renderLineViewDoc,

    -- * Note Docs
    renderNotesDoc,
    impasseNote,
    actionExceptionNote,
    propertyKindNote,
    propertyNote,
    stepNote,
    worldNote,
    stutterNote,
    infiniteStutterNote,
    finiteStutterNote,
    cyclicBehaviorNote,
  )
where

import Data.Text.Prettyprint.Doc
  ( Doc,
    Pretty (pretty),
    align,
    annotate,
    enclose,
    hang,
    hardline,
    indent,
    vsep,
    (<+>),
  )
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, Color (Green, Red, White), bold, color, italicized)
import GHC.Stack (SrcLoc (srcLocFile, srcLocStartLine))
import Lens.Micro ((^.))

import Data.Type.Rec (Rec)
import Language.Spectacle.Checker.Metrics
  ( ModelMetrics,
    distinctStates,
    treeDepth,
    treeWidth,
  )
import Language.Spectacle.Checker.Model.MCError
  ( InternalErrorKind (EmptyDisjunctQueueK),
    MCError
      ( MCActionError,
        MCFormulaError,
        MCFormulaRuntimeError,
        MCImpasseError,
        MCInitialError,
        MCInternalError,
        MCNoInitialStatesError,
        MCStrongLivenessError,
        MCStutterError
      ),
    PropertyKind (AlwaysPropK, EventuallyPropK, InfinitelyOftenPropK, StaysAsPropK, UpUntilPropK),
    StutterKind (FiniteStutterK, InfiniteStutterK),
  )
import Language.Spectacle.Checker.Step (Step)
import Language.Spectacle.Checker.World (World)
import Language.Spectacle.Exception.RuntimeException (RuntimeException)

-- ----------------------------------------------------------------------------------------------------------------------
-- Model Result Documents
--
-- Section for rendering model metrics
--

renderModelErrorsDoc :: Show (Rec ctx) => [MCError ctx] -> IO (Doc AnsiStyle)
renderModelErrorsDoc errs = do
  errDocs <- vsep <$> mapM renderMCError errs
  return (errDocs <> hardline <> lastLine <> hardline)
  where
    lastLine :: Doc AnsiStyle
    lastLine = "Model checking failed with" <+> annotate (bold <> color Red) (pretty (length errs)) <+> "errors."

renderModelMetrics :: ModelMetrics -> Doc AnsiStyle
renderModelMetrics metrics =
  annotate (italicized <> color Green) "Success"
    <> ","
    <+> "model checker satisfied all properties with"
    <> hardline
    <> renderNotesDoc
      2
      [ annotate (bold <> color White) (pretty (metrics ^. distinctStates)) <+> "distinct states"
      , "A maximum search depth of" <+> annotate (bold <> color White) (pretty (metrics ^. treeDepth))
      , "A maximum search tree width of" <+> annotate (bold <> color White) (pretty (metrics ^. treeWidth))
      ]
    <> hardline

-- ----------------------------------------------------------------------------------------------------------------------
-- Error Documents
--
-- Section for functions which produce fully-formed errors.
--

renderMCError :: Show (Rec ctx) => MCError ctx -> IO (Doc AnsiStyle)
renderMCError = \case
  MCInitialError exc -> return (renderMCInitialErrorDoc exc)
  MCNoInitialStatesError -> return renderMCNoInitialStatesErrorDoc
  MCActionError world exc -> return (renderMCActionErrorDoc world exc)
  MCImpasseError world -> return (renderMCImpasseErrorDoc world)
  MCStutterError step srcLoc propK stutterK -> renderMCStutterErrorDoc step srcLoc propK stutterK
  MCFormulaError step srcLoc propK -> renderMCFormulaErrorDoc step srcLoc propK
  MCFormulaRuntimeError step exc -> return (renderMCFormulaRuntimeErrorDoc step exc)
  MCStrongLivenessError srcLoc -> renderMCStrongLivenessErrorDoc srcLoc
  MCInternalError errorK -> return (renderMCInternalErrorDoc errorK)

renderMCInitialErrorDoc :: RuntimeException -> Doc AnsiStyle
renderMCInitialErrorDoc exc =
  vsep
    [ annotate (bold <> color White) (initialLocDoc <> ":") <+> errorDoc
    , renderNotesDoc 2 [initialExceptionNote exc]
    ]

renderMCNoInitialStatesErrorDoc :: Doc AnsiStyle
renderMCNoInitialStatesErrorDoc =
  vsep
    [ annotate (bold <> color White) (initialLocDoc <> ":") <+> errorDoc
    , renderNotesDoc 2 ["The initial action produced no initial states."]
    ]

renderMCImpasseErrorDoc :: Show (Rec ctx) => World ctx -> Doc AnsiStyle
renderMCImpasseErrorDoc world =
  vsep
    [ annotate (bold <> color White) (actionLocDoc <> ":") <+> errorDoc
    , renderNotesDoc 2 [impasseNote world]
    ]

renderMCActionErrorDoc :: Show (Rec ctx) => World ctx -> RuntimeException -> Doc AnsiStyle
renderMCActionErrorDoc world exc =
  vsep
    [ annotate (bold <> color White) (actionLocDoc <> ":") <+> errorDoc
    , renderNotesDoc 2 [actionExceptionNote world exc]
    ]

renderMCStutterErrorDoc ::
  Show (Rec ctx) =>
  Step ctx ->
  Maybe SrcLoc ->
  PropertyKind ->
  StutterKind ->
  IO (Doc AnsiStyle)
renderMCStutterErrorDoc step Nothing propK stutterK = do
  return . vsep $
    [ annotate (bold <> color White) (unknownLocDoc <> ":") <+> errorDoc
    , renderNotesDoc
        2
        [ stutterNote stutterK
        , propertyKindNote propK <> "," <+> propertyNote propK
            <> hardline
            <> indent 2 (stepNote step)
        ]
        <> hardline
    ]
renderMCStutterErrorDoc step (Just srcLoc) propK stutterK = do
  let padding = length (show (srcLocStartLine srcLoc))
  lineView <- renderLineViewDoc (srcLocStartLine srcLoc) (srcLocFile srcLoc)
  return . vsep $
    [ annotate (bold <> color White) (fileHeadingDoc (srcLocFile srcLoc) <> ":") <+> errorDoc
    , renderNotesDoc
        padding
        [ stutterNote stutterK
        , propertyKindNote propK <> "," <+> propertyNote propK
            <> hardline
            <> indent 2 (stepNote step)
        ]
    , hang (- 2) lineView <> hardline
    ]

renderMCFormulaErrorDoc ::
  Show (Rec ctx) =>
  Step ctx ->
  Maybe SrcLoc ->
  PropertyKind ->
  IO (Doc AnsiStyle)
renderMCFormulaErrorDoc step Nothing propK =
  return . vsep $
    [ annotate (bold <> color White) (unknownLocDoc <> ":") <+> errorDoc
    , renderNotesDoc
        2
        [ propertyKindNote propK <> "," <+> propertyNote propK
            <> hardline
            <> indent 2 (stepNote step)
        ]
        <> hardline
    ]
renderMCFormulaErrorDoc step (Just srcLoc) propK = do
  let padding = length (show (srcLocStartLine srcLoc))
  lineView <- renderLineViewDoc (srcLocStartLine srcLoc) (srcLocFile srcLoc)
  return . vsep $
    [ annotate (bold <> color White) (fileHeadingDoc (srcLocFile srcLoc) <> ":") <+> errorDoc
    , renderNotesDoc
        padding
        [ propertyKindNote propK <> "," <+> propertyNote propK
            <> hardline
            <> indent 2 (stepNote step)
        ]
    , hang (- 2) lineView <> hardline
    ]

renderMCFormulaRuntimeErrorDoc :: Show (Rec ctx) => Step ctx -> RuntimeException -> Doc AnsiStyle
renderMCFormulaRuntimeErrorDoc step exc =
  vsep
    [ annotate (bold <> color White) (actionLocDoc <> ":") <+> errorDoc
    , renderNotesDoc 2 [formulaExceptionNote step exc] <> hardline
    ]

renderMCStrongLivenessErrorDoc :: Maybe SrcLoc -> IO (Doc AnsiStyle)
renderMCStrongLivenessErrorDoc = \case
  Nothing ->
    return $
      unknownLocDoc
        <> ":" <+> errorDoc
        <> hardline
        <> indent 2 "could not satisfy liveness properties with strong fairness constraint"
  Just srcLoc -> do
    let padding = length (show (srcLocStartLine srcLoc))
    lineView <-
      renderLineViewDoc
        (srcLocStartLine srcLoc)
        (srcLocFile srcLoc)
    return . vsep $
      [ annotate (bold <> color White) (fileHeadingDoc (srcLocFile srcLoc) <> ":") <+> errorDoc
      , indent padding "could not satisfy liveness properties with strong fairness constraint"
      , hang (- 2) lineView <> hardline
      ]

renderMCInternalErrorDoc :: InternalErrorKind -> Doc AnsiStyle
renderMCInternalErrorDoc = \case
  EmptyDisjunctQueueK -> errorDoc <+> "disjunct queue ran out of branches while interpreting temporal formula"

errorDoc :: Doc AnsiStyle
errorDoc = annotate (bold <> color Red) "error:"
{-# INLINE CONLIKE errorDoc #-}

-- ----------------------------------------------------------------------------------------------------------------------
-- File Views
--
-- "File views" are any document which render the contents of the file where the error occured.
--

fileHeadingDoc :: String -> Doc AnsiStyle
fileHeadingDoc filePath = annotate (bold <> color White) (pretty filePath)
{-# INLINE fileHeadingDoc #-}

initialLocDoc :: Doc AnsiStyle
initialLocDoc = "<initial action>"
{-# INLINE CONLIKE initialLocDoc #-}

unknownLocDoc :: Doc AnsiStyle
unknownLocDoc = "<unknown source location>"
{-# INLINE CONLIKE unknownLocDoc #-}

-- | Errors that have to do with the model's action such as a runtime exception emit the location "<model action>"
-- rather than a specific file path.
--
-- @since 0.1.0.0
actionLocDoc :: Doc AnsiStyle
actionLocDoc = "<model action>"
{-# INLINE CONLIKE actionLocDoc #-}

-- | Errors that have to do with the model's formula such as a syntax exception emit the location "<model formula>"
-- rather than a specific file path.
--
-- @since 0.1.0.0
formulaLocDoc :: Doc AnsiStyle
formulaLocDoc = "<model formula>"
{-# INLINE CONLIKE formulaLocDoc #-}

-- | Produces a single-line view of the source file annotated with the line number it was taken from.
--
-- @since 0.1.0.0
renderLineViewDoc :: Int -> String -> IO (Doc AnsiStyle)
renderLineViewDoc lineNumber filePath = do
  srcLines <- lines <$> readFile filePath
  let line = srcLines !! (lineNumber - 1)
      padding = length (show lineNumber) + 1 -- lineNumber
  return . vsep $
    [ indent padding (annotate (bold <> color Red) "│")
    , annotate bold (pretty lineNumber) <+> annotate (bold <> color Red) "│" <+> pretty line
    , indent padding (annotate (bold <> color Red) "│")
    ]
{-# INLINE renderLineViewDoc #-}

-- ----------------------------------------------------------------------------------------------------------------------
-- Note Documents
--
-- "Notes" are categorized as anything which can appear as a bulleted item directly under an error's heading to elaborate
-- on the error being rendered.
--

-- | Renders a list of notes as bullet points at a given indentation level.
--
-- @since 0.1.0.0
renderNotesDoc :: Int -> [Doc AnsiStyle] -> Doc AnsiStyle
renderNotesDoc indentation =
  indent indentation . vsep . map \note -> annotate (bold <> color White) "*" <+> align (annotate (color White) note)
{-# INLINE renderNotesDoc #-}

impasseNote :: Show (Rec ctx) => World ctx -> Doc AnsiStyle
impasseNote world =
  annotate italicized "impasse," <+> "there are no reachable states to continue from the world"
    <> hardline
    <> indent 2 (worldNote world)
{-# INLINE impasseNote #-}

initialExceptionNote :: RuntimeException -> Doc AnsiStyle
initialExceptionNote exc =
  vsep
    [ annotate italicized "exception thrown," <+> "exception raised while evaluating the initial action"
    , indent 2 (annotate (bold <> color Red) (enclose hardline hardline (pretty (show exc))))
    ]
{-# INLINE initialExceptionNote #-}

actionExceptionNote :: Show (Rec ctx) => World ctx -> RuntimeException -> Doc AnsiStyle
actionExceptionNote world exc =
  vsep
    [ annotate italicized "exception thrown," <+> "cannot reach new states due to the exception"
    , indent 2 (annotate (bold <> color Red) (enclose hardline hardline (pretty (show exc))))
    , "being thrown for the world"
    , indent 2 (worldNote world)
    ]
{-# INLINE actionExceptionNote #-}

formulaExceptionNote :: Show (Rec ctx) => Step ctx -> RuntimeException -> Doc AnsiStyle
formulaExceptionNote step exc =
  vsep
    [ annotate italicized "exception thrown," <+> "cannot reach new states due to the exception"
    , indent 2 (annotate (bold <> color Red) (enclose hardline hardline (pretty (show exc))))
    , "being thrown for the world"
    , indent 2 (stepNote step)
    ]
{-# INLINE formulaExceptionNote #-}

propertyKindNote :: PropertyKind -> Doc AnsiStyle
propertyKindNote propK = annotate italicized propMessage
  where
    propMessage :: Doc AnsiStyle
    propMessage = case propK of
      AlwaysPropK -> "always violated"
      EventuallyPropK -> "eventually violated"
      UpUntilPropK -> "up until violated"
      StaysAsPropK -> "stays as violated"
      InfinitelyOftenPropK -> "infinitely often violated"
{-# INLINE propertyKindNote #-}

propertyNote :: PropertyKind -> Doc AnsiStyle
propertyNote = \case
  AlwaysPropK -> "invariant property is false for the step"
  EventuallyPropK -> "liveness property is false for all actions following the step"
  UpUntilPropK -> "the left condition does not hold up until the right condition is satisfied for the step"
  StaysAsPropK -> "the property never remains true for all actions following the step"
  InfinitelyOftenPropK -> "the property is never satisfied following the step"
{-# INLINE propertyNote #-}

stepNote :: Show (Rec ctx) => Step ctx -> Doc AnsiStyle
stepNote = annotate (bold <> color White) . enclose hardline hardline . pretty . show
{-# INLINE stepNote #-}

worldNote :: Show (Rec ctx) => World ctx -> Doc AnsiStyle
worldNote = annotate (bold <> color White) . enclose hardline hardline . pretty . show
{-# INLINE worldNote #-}

stutterNote :: StutterKind -> Doc AnsiStyle
stutterNote InfiniteStutterK = infiniteStutterNote
stutterNote FiniteStutterK = finiteStutterNote

infiniteStutterNote :: Doc AnsiStyle
infiniteStutterNote = "Occurring in an infinite sequence of stutter-steps"
{-# INLINE CONLIKE infiniteStutterNote #-}

finiteStutterNote :: Doc AnsiStyle
finiteStutterNote = "Occurring in a stutter-step"
{-# INLINE CONLIKE finiteStutterNote #-}

cyclicBehaviorNote :: Doc AnsiStyle
cyclicBehaviorNote = "In the cyclic behavior"
{-# INLINE CONLIKE cyclicBehaviorNote #-}
