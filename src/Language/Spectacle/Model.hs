{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Model where

import Control.Comonad.Cofree ()
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader
  ( MonadReader,
    ReaderT (runReaderT),
    asks,
  )
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Foldable (for_)
import Data.Hashable (Hashable)
import Data.Map (Map)
import qualified Data.Map as Map
import Lens.Micro ((^.))
import Lens.Micro.Mtl (view)
import Data.Traversable
import Control.Applicative
import Control.Monad.Except (MonadError (throwError))

import Data.Fingerprint
import Data.Functor.Tree (Tree, breadth)
import Data.Type.Rec (HasDict)
import Data.World (World, fingerprint)
import qualified Debug.Trace as Debug
import Language.Spectacle.Interaction.Diagram
  ( diagramFull,
    runDiagram,
  )
import qualified Language.Spectacle.Interaction.Paths as Paths
import Language.Spectacle.Model.MCError (MCError)
import qualified Language.Spectacle.Model.MCError as MCError
import Language.Spectacle.Model.MCState as MCState
  ( MCState,
    enabled,
    entryAt,
    lookup,
  )
import Language.Spectacle.Model.Trace
  ( RSet (RSet),
    runTraceM,
    traceModelTree,
    traceRSets,
  )
import Language.Spectacle.Specification
  ( ActionSpine (..),
    HasActions (takeActionSpine),
    HasVariables (VarCtxt),
    Spec (..),
    Specification,
    actionDeclFair,
    actionDeclName,
    fromActionDecl,
    runInitStates,
  )
import Language.Spectacle.Specification.Prop
  ( HasProp (collectPropInfo),
    PropInfo (propInfoIsAlways),
  )
import Prettyprinter.Render.Terminal (putDoc)

-- ---------------------------------------------------------------------------------------------------------------------

modelcheck ::
  ( HasDict Hashable ctx
  , HasDict Show ctx
  , Specification vars spec prop
  , ctx ~ VarCtxt vars
  ) =>
  Spec vars spec prop ->
  IO (Either MCError [Tree (World ctx)])
modelcheck spec = runExceptT (runModelCheck spec)

runModelCheck ::
  forall ctx vars spec prop.
  ( HasDict Hashable ctx
  , HasDict Show ctx
  , Specification vars spec prop
  , ctx ~ VarCtxt vars
  ) =>
  Spec vars spec prop ->
  ExceptT MCError IO [Tree (World ctx)]
runModelCheck (Spec vars acts) = do
  let rsets = lowerActions acts
  let initials = runInitStates vars
  let propInfo = collectPropInfo @prop

  when (null initials) do
    throwE MCError.NoInitState

  (modelst, modelTrees) <- do
    let machine = traceRSets rsets
    (st, results) <- runTraceM (traverse (traceModelTree machine) initials)
    case results of
      Left exc -> throwE exc
      Right trees -> pure (st, trees)

  -- for modelTrees \model -> do
  --   _

  _ <- runReaderT (checkProps propInfo modelTrees) modelst

  let pointset = Paths.toPointSet (head modelTrees)
  let !_ = Debug.trace ("set size:  " ++ show (length pointset)) ()
  let !_ = Debug.trace ("tree size: " ++ show (length (head modelTrees))) ()
  doc <- liftIO $ runDiagram (diagramFull pointset)
  liftIO $ putDoc doc

  liftIO $ print modelst
  undefined

lowerActions :: forall ctx acts. HasActions ctx acts => acts ctx -> [RSet ctx]
lowerActions = go . takeActionSpine
  where
    go :: ActionSpine ctx acts' -> [RSet ctx]
    go = \case
      ActionSpineNil -> []
      ActionSpineCon act sp ->
        let name = actionDeclName act
            fair = actionDeclFair act
         in RSet name fair (fromActionDecl act) : go sp

checkProps ::
  (MonadError MCError m, MonadReader MCState m) =>
  Map String PropInfo ->
  [Tree (World ctx)] ->
  m [Tree (World ctx)]
checkProps propInfo =
  traverse \modelTree -> do
    let globals = globalActionsOf propInfo
    for_ globals \name -> do
      checkGlobal name modelTree
    pure modelTree

globalActionsOf :: Map String PropInfo -> [String]
globalActionsOf = map fst . Map.toList . Map.filter propInfoIsAlways

checkGlobal ::
  (MonadError MCError m, MonadReader MCState m) =>
  String ->
  Tree (World ctx) ->
  m (Tree (World ctx))
checkGlobal name =
  breadth \world -> do
    node <- asks $ MCState.lookup (world ^. fingerprint)
    let isEnabled = view (entryAt name . enabled) node
    if isEnabled
      then pure world
      else throwError (MCError.FailAlways name)
