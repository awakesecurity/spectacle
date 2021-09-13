{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Language.Spectacle.Interaction
  ( defaultInteraction,
  )
where

import Control.Monad (forM_, forM, when)
import Control.Monad.Except
  ( ExceptT,
    MonadIO (liftIO),
    runExceptT,
  )
import Data.Function ((&))
import Data.Hashable (Hashable)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text.Prettyprint.Doc
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO
  ( BufferMode (LineBuffering),
    hSetBuffering,
    stderr,
    stdout,
  )
import Text.Megaparsec (runParser, (<|>))
import Lens.Micro
import Lens.Micro.Mtl

import Control.Monad.Levels
  ( LevelsT,
    foldMapAp,
    forAp,
    runLevelsA,
  )
import Data.Type.Rec (Rec)
import Data.World (World, worldFingerprint)
import Language.Spectacle.Checker (modelCheck)
import Language.Spectacle.Checker.Fingerprint
import Language.Spectacle.Checker.MCError (MCError)
import Language.Spectacle.Checker.Model (modelNextSets)
import Language.Spectacle.Checker.Replayer
  ( Behavior (Behavior),
    replayModelTrace,
  )
import Language.Spectacle.Interaction.Parse
  ( ReplayOpts (ReplayOpts, replayDepth, replayFrom, replayTo),
    parseReplayOpts,
  )
import Language.Spectacle.Interaction.Render
  ( renderMCMetrics,
    renderModelErrorsDoc,
  )
import Language.Spectacle.Interaction.View.WorldView
  ( ViewableSignature,
    newWorldView,
    ppWorldView,
  )
import Language.Spectacle.Specification
  ( ActionSet (ActionSet, actionSetName, actionSetWorlds),
    ActionSpine,
    HasActions (ActionCtxt, takeActionSpine),
    HasVariables (VariableCtxt),
    Spec (Spec),
    Specification,
    specInitialWorlds,
  )

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'defaultInteraction' is an 'IO' action which handles rendering model failures or success to terminal. Given some
-- specification @spec@
--
-- @
-- main :: IO ()
-- main = defaultIntraction (modelCheck spec)
-- @
--
-- is all that is needed to perform model checks and output the results.
--
-- @since 0.1.0.0
defaultInteraction ::
  forall vars spec prop ctxt acts.
  ( Specification vars spec prop
  , VariableCtxt vars ~ ctxt
  , ActionCtxt ctxt spec ~ acts
  , ViewableSignature ctxt
  , Show (Rec ctxt)
  , Hashable (Rec ctxt)
  ) =>
  Spec vars spec prop ->
  IO ()
defaultInteraction spec = do
  args <- getArgs

  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  if null args
    then case modelCheck spec of
      Left errs -> do
        putDoc =<< renderModelErrorsDoc errs
        exitFailure
      Right metrics -> do
        putDoc (renderMCMetrics metrics)
        exitSuccess
    else case runParser parseReplayOpts "stdin" (unwords args) of
      Left errs -> do
        print errs
        exitFailure
      Right ReplayOpts {..} ->
        case replayModelTrace replayFrom replayTo replayDepth spec of
          Left errs -> do
            putDoc =<< renderModelErrorsDoc errs
            exitFailure
          Right excTrace -> emitReplayTrace replayFrom replayTo spec excTrace

data HoareTriple ctxt = HoareTriple
  { hoarePrecond :: World ctxt
  , hoareAction :: String
  , hoarePostcond :: World ctxt
  }
  deriving (Eq, Ord)

emitReplayTrace ::
  forall vars spec prop ctxt acts.
  ( Specification vars spec prop
  , VariableCtxt vars ~ ctxt
  , ActionCtxt ctxt spec ~ acts
  , ViewableSignature ctxt
  , Show (Rec ctxt)
  , Hashable (Rec ctxt)
  ) =>
  Fingerprint ->
  Fingerprint ->
  Spec vars spec prop ->
  Set Behavior ->
  IO ()
emitReplayTrace fpFrom fpTo spec@(Spec _ sp) behavior' = do
  emit <-
    traceBFS initialWorlds behavior'
      & runLevelsA
      & runExceptT

  case emit of
    Left errs -> do


      putDoc =<< renderModelErrorsDoc errs
    Right _ -> return ()
  where
    spine :: ActionSpine ctxt acts
    spine = takeActionSpine sp

    initialWorlds :: Set (World ctxt)
    initialWorlds = specInitialWorlds spec

    traceBFS ::
      Set (World ctxt) ->
      Set Behavior ->
      LevelsT (ExceptT [MCError ctxt] IO) ()
    traceBFS worldsHere behavior = case Set.minView behavior of
      Nothing -> return ()
      Just (Behavior depth actions, xs) -> do
        triples <- forAp worldsHere \worldHere -> do
          nexts <- modelNextSets worldHere spine

          when (depth == 1) do
            let worldView = newWorldView worldHere
                fpHere = view worldFingerprint worldHere
                worldDoc =
                  if fpHere == fpTo || fpHere == fpFrom
                    then annotate (bold <> color Red) (ppWorldView 0 "<initial>" fpHere worldView <> hardline)
                    else ppWorldView 0 "<initial>" fpHere worldView <> hardline

            liftIO (putDoc worldDoc)

          forAp nexts \ActionSet {..} -> do
            if actionSetName `elem` actions
              then return (Set.map (HoareTriple worldHere actionSetName) actionSetWorlds)
              else return Set.empty

        liftIO do
          forM_ triples \HoareTriple {..} -> do
            let worldView = newWorldView hoarePostcond
                fpPrecond = view worldFingerprint hoarePrecond
                fpPostcond = view worldFingerprint hoarePostcond

                worldDoc =
                  if fpPostcond == fpTo || fpPostcond == fpFrom
                    then annotate (bold <> color Red) (ppWorldView depth hoareAction fpPrecond worldView <> hardline)
                    else ppWorldView depth hoareAction fpPrecond worldView <> hardline

            putDoc worldDoc

        return () <|> traceBFS (Set.map hoarePostcond triples) xs
