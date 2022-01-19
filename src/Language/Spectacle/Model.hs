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

-- initialTimes ::
--   (ctxt ~ VariableCtxt vars, Hashable (Rec ctxt), HasVariables vars) =>
--   vars ->
--   Either [MCError ctxt] (Set (World ctxt))
-- initialTimes spec = do
--   let sets =
--         takeInitialActions spec
--           & Rec.mapF (const $ runLang . runNonDetA @[])
--           & seqRec
--           & foldMap (Set.singleton . makeWorld)

--   if Set.null sets
--     then throwError [MCNoInitialStatesError]
--     else return sets
--   where
--     seqRec :: RecF [] ctxt -> [Rec ctxt]
--     seqRec = \case
--       NilF -> [Rec.Nil]
--       ConF name states xs -> liftA2 (Rec.Con name) states (seqRec xs)

-- -- | Setting up necessary action information mapped by the actions name.
-- --
-- -- @since 0.1.0.0
-- setupActionInfo ::
--   [(Fairness, String, Action ctxt Bool)] ->
--   Map String PropInfo ->
--   Map String (Fairness, PropInfo)
-- setupActionInfo acts propInfos =
--   let elements :: [(Fairness, PropInfo)]
--       elements = foldMap (\(fair, name, _) -> [(fair, propInfos Map.! name)]) acts

--       keyIdents :: [String]
--       keyIdents = foldMap (\(_, name, _) -> [name]) acts
--    in Map.fromList (zip keyIdents elements)

-- -- | Taking the actions from a specification of actions @acts ctxt@.
-- --
-- -- @since 0.1.0.0
-- setupActions ::
--   (Hashable (Rec ctxt), HasActions ctxt acts) =>
--   acts ctxt ->
--   [(Fairness, String, Action ctxt Bool)]
-- setupActions = reflectActs . takeActionSpine
--   where
--     reflectActs ::
--       Hashable (Rec ctxt) =>
--       ActionSpine ctxt acts ->
--       [(Fairness, String, Action ctxt Bool)]
--     reflectActs = \case
--       ActionSpineNil -> []
--       ActionSpineCon actDecl acts ->
--         let act = case actDecl of
--               UnfairAction name f -> (Unfair, show name, f)
--               WeakFairAction name f -> (WeakFair, show name, f)
--               StrongFairAction name f -> (StrongFair, show name, f)
--          in act : reflectActs acts

-- runModelCheck ::
--   forall vars spec prop ctxt acts.
--   ( Specification vars spec prop
--   , ctxt ~ VariableCtxt vars
--   , acts ~ ActionCtxt ctxt spec
--   , Show (Rec ctxt)
--   , Hashable (Rec ctxt)
--   ) =>
--   Spec vars spec prop ->
--   IO (Either [MCError ctxt] [Tree (World ctxt)])
-- runModelCheck spec@(Spec vs sp) = do
--   let actions = setupActions sp
--       actInfo = setupActionInfo actions (collectPropInfo @prop)
--       tabula = (setupTabula `on` Set.fromList) (map (view _2) actions) (Map.keys actInfo)

--   case Set.toList <$> initialTimes vs of
--     Left err -> return (Left err)
--     Right initWorlds -> do
--       result <-
--         iterPathsM actions (Set.fromList initWorlds)
--           & flip evalStateT tabula
--           & runMaybeT

--       case result of
--         Nothing -> error "nothing!!!"
--         Just tree -> do
--           pure (Right tree)

-- iterPathsM ::
--   forall m ctxt.
--   (MonadIO m, Show (Rec ctxt), Hashable (Rec ctxt)) =>
--   [(Fairness, String, Action ctxt Bool)] ->
--   Set (World ctxt) ->
--   m [Tree (World ctxt)]
-- iterPathsM actions =
--   let pathsOf :: World ctxt -> StateT (Set Fingerprint) m (Tree (World ctxt))
--       pathsOf world@(World fp vals) = StateT \tabula ->
--         if Set.member fp tabula
--           then pure (Leaf world, tabula)
--           else do
--             let leaves = Set.unions (map (runAction vals . view _3) actions)
--                 tabula' = Set.insert fp tabula

--             (xs, st) <- runStateT (foldr (liftA2 (:) . pathsOf) (pure []) leaves) tabula'
--             pure (world :- xs, tabula')

--       foldPaths :: [World ctxt] -> m [Tree (World ctxt)]
--       foldPaths = traverse \world -> do
--         (tree, _) <- runStateT (pathsOf world) Set.empty
--         pure tree
--   in foldPaths . Set.toList

--- breadthFirstM \(depth, Equation oldDom oldCod) -> do
--   liftIO (putStrLn ("(" ++ show depth ++ "): " ++ show oldDom))
--   liftIO (putStrLn ("(" ++ show depth ++ "): " ++ show oldCod))
--   liftIO (putStrLn "--")

--   if isNothing node
--     then setFor (Set.unions (IntMap.elems oldCod)) (iterator depth)
--     else pure Set.empty
-- where
--   iterator :: Int -> World ctxt -> m (Int, EquationW ctxt)
--   iterator depth world = do
--     mappings <- forM actions \(_, actName, action) -> do
--       let newCod = runAction (view worldValues world) action
--       actIdx <- gets (ixAction actName)

--       pure (actIdx, newCod)

--     pure (depth + 1, Equation world (IntMap.fromList mappings))

-- data Equation a = Equation
--   { equationDom :: a
--   , equationCod :: IntMap (Set a)
--   }
--   deriving (Eq, Ord, Show)

-- -- | Synonym for concrete world equations.
-- --
-- -- @since 0.1.0.0
-- type EquationW ctxt = Equation (World ctxt)

-- -- | Synonym for fingerprint equations.
-- --
-- -- @since 0.1.0.0
-- type EquationNodeF = Equation Fingerprint

-- let newDom = Set.unions (IntMap.elems (equationCod equation))

-- xs <- setFor newDom \world -> do
--   mappings <- forM actions \(_, actName, action) -> do
--     let newCod = runAction (view worldValues world) action
--     actIdx <- gets (ixAction actName)

--     liftIO (putStrLn ("(" ++ show d ++ "): " ++ show world))
--     liftIO (putStrLn ("(" ++ show d ++ "): " ++ show newCod))
--     liftIO (putStrLn "--")

--     pure (actIdx, newCod)

--   ixWorld world %= (<> Just (TabulaNode Set.empty IntMap.empty IntMap.empty))
--   pure (d + 1, Equation world (IntMap.fromList mappings))

-- pure xs

-- testResult <-
--   runContT (checkerLoop initWorlds (setupChecks actInfo actions)) pure
--     & observeLevelsT
--     & flip runStateT tabula
-- -- runCoK (checkLoop 0 actions actInfo initWorlds) (wrap . pure)

-- print "result:"
-- -- print (fst testResult)
-- pure (Right ())

-- checkerLoop ::
--   (MonadIO m, MonadState Tabula m, Show (World ctxt)) =>
--   Set (World ctxt) ->
--   [Bool -> World ctxt -> ContT (Set (World ctxt)) (LevelsT m) (String, Set (World ctxt), Bool)] ->
--   ContT (Set (World ctxt)) (LevelsT m) (Set (World ctxt))
-- checkerLoop worlds checks = ContT \k -> do
--   worldsWithTerm <- forAp worlds \world -> do
--     terminate <- maybe False (not . Set.null . view nodeNexts) <$> use (ixWorld world)
--     pure [(world, terminate)]

--   nextWorlds <-
--     forAp checks \check -> do
--       forAp worldsWithTerm \(world, terminate) -> do
--         nexts <- runContT (check terminate world) \(actName, xs, truth) -> do
--           tabulateStep actName world xs truth
--           k xs

--         pure [(terminate, world, nexts)]

--   nextQueue <-
--     Set.unions <$> for nextWorlds \(terminate, worldHere, worldsThere) ->
--       if terminate
--         then pure Set.empty
--         else pure worldsThere

--   liftIO (print nextQueue)
--   if Set.null nextQueue
--     then pure worlds
--     else pure worlds <|> runContT (checkerLoop nextQueue checks) k

-- tabulateStep :: (Alternative m, MonadState Tabula m) => String -> World ctxt -> Set (World ctxt) -> Bool -> m ()
-- tabulateStep actIdent worldHere worldsThere truth = do
--   propIdx <- use (propBinderIdx actIdent)
--   actIdx <- gets (ixAction actIdent)
--   case propIdx of
--     Nothing -> empty
--     Just idx -> do
--       let neighbors = Set.map (view worldFingerprint) worldsThere
--           propTruth = IntMap.singleton idx truth
--           enabled = IntMap.singleton actIdx (not (Set.null worldsThere))

--       ixWorld worldHere %= (<> Just (TabulaNode neighbors propTruth enabled))

-- setupChecks ::
--   (MonadIO m, MonadState Tabula m, Show (Rec ctxt), Hashable (Rec ctxt)) =>
--   Map String (Fairness, PropInfo) ->
--   [(Fairness, String, Action ctxt Bool)] ->
--   [Bool -> World ctxt -> ContT (Set (World ctxt)) (LevelsT m) (String, Set (World ctxt), Bool)]
-- setupChecks actInfo = map \(fair, actName, action) terminate world -> do
--   case snd <$> Map.lookup actName actInfo of
--     Nothing -> lift empty
--     Just propInfo
--       | propInfoIsAlways propInfo -> do
--         let nexts = runAction (view worldValues world) action
--         (nexts', truth) <- globalM world nexts
--         pure (actName, nexts', truth)
--       | propInfoIsEventually propInfo -> do
--         let nexts = runAction (view worldValues world) action
--         prevTruth <- fromMaybe False <$> lift (gets (ixTruth actName world))
--         (nexts', truth) <- futureM fair prevTruth terminate world nexts

--         forAp nexts' \next -> do
--           propIdx <- use (propBinderIdx actName)
--           case propIdx of
--             Nothing -> pure ()
--             Just idx -> do
--               let propTruth = IntMap.singleton idx truth
--               ixWorld world %= (<> Just (TabulaNode mempty propTruth mempty))

--         return (actName, nexts', truth)
--       | propInfoIsInfinitelyOften propInfo -> undefined
--       | propInfoIsStaysAs propInfo -> undefined
--       | not (Set.null (propInfoLeadsTo propInfo)) -> undefined
--       | otherwise -> undefined

-- globalF ::
--   (MonadIO m, Alternative m, MonadState (Set Fingerprint) m, Show (Rec ctxt)) =>
--   (World ctxt -> Set (World ctxt)) ->
--   World ctxt ->
--   S m (Set (World ctxt))
-- globalF f w = S \p -> do
--   pw <- p (f w)

--   liftIO (putStrLn ("G: " ++ show w))

--   pure (f w) <|> Set.unions <$> forM (Set.toList (f w)) \w' -> do
--     marked <- gets (Set.member (view worldFingerprint w'))
--     if marked
--       then pure Set.empty
--       else findS (globalF f w') p

-- futureF ::
--   (MonadIO m, Alternative m, MonadState (Set Fingerprint) m, Show (Rec ctxt)) =>
--   (World ctxt -> Set (World ctxt)) ->
--   World ctxt ->
--   S m (Set (World ctxt))
-- futureF f w = S \p -> do
--   let ws = f w
--   pw <- p ws

--   liftIO (putStrLn ("F: " ++ show w))

--   pure (f w) <|> Set.unions <$> forM (Set.toList ws) \w' -> do
--     marked <- gets (Set.member (view worldFingerprint w'))
--     if marked
--       then pure Set.empty
--       else findS (futureF f w') p

-- newtype Temporal :: Type -> Type -> (Type -> Type) -> Type -> Type where
--   Temporal :: {getTemporal :: ReaderT e (ContT r m) a} -> Temporal r e m a
--   deriving (Functor, Applicative, Monad)

-- globalM ::
--   (MonadIO m, MonadState Tabula m, Show (Rec ctxt)) =>
--   World ctxt ->
--   Set (World ctxt) ->
--   ContT (Set (World ctxt)) (LevelsT m) (Set (World ctxt), Bool)
-- globalM here there
--   | Set.null there = do
--     isMarked <- isJust <$> lift (use (ixWorld here))
--     if isMarked
--       then lift empty
--       else error "always error"
--   | otherwise = do
--     pure (there, Set.null there)

-- futureM ::
--   (MonadIO m, MonadState Tabula m, Show (Rec ctxt)) =>
--   Fairness ->
--   Bool ->
--   Bool ->
--   World ctxt ->
--   Set (World ctxt) ->
--   ContT (Set (World ctxt)) (LevelsT m) (Set (World ctxt), Bool)
-- futureM fairness prevTruth terminate here there
--   | terminate = case fairness of
--     Unfair
--       | prevTruth || not (Set.null there) -> do
--         node <- use (ixWorld here)

--         liftIO (putStrLn ("t:  " ++ show here))
--         liftIO (putStrLn ("t': " ++ show there))
--         liftIO (putStrLn ("previous: " ++ show prevTruth))
--         liftIO (putStrLn ("to terminate: " ++ show terminate))
--         liftIO (putStrLn ("terminate state: " ++ show node))

--         pure (there, True)
--       | otherwise -> do
--         node <- use (ixWorld here)

--         liftIO (putStrLn ("t:  " ++ show here))
--         liftIO (putStrLn ("t': " ++ show there))
--         liftIO (putStrLn ("previous: " ++ show prevTruth))
--         liftIO (putStrLn ("to terminate: " ++ show terminate))
--         liftIO (putStrLn ("terminate state: " ++ show node))

--         error "future error, unfair!"
--     WeakFair -> undefined
--     StrongFair -> undefined
--   | otherwise = do
--     node <- use (ixWorld here)

--     liftIO (putStrLn ("t:  " ++ show here))
--     liftIO (putStrLn ("t': " ++ show there))
--     liftIO (putStrLn ("previous: " ++ show prevTruth))
--     liftIO (putStrLn ("to terminate: " ++ show terminate))
--     liftIO (putStrLn ("step state: " ++ show node))

--     pure (there, prevTruth || not (Set.null there))

-- -- node <- use (ixWorld here)
-- -- case node of
-- --   Nothing -> pure (there, not (Set.null there) || prevTruth)
-- --   Just info
-- --     | prevTruth || not (Set.null there) -> pure (there, True)
-- --     | otherwise -> case fairness of
-- --       Unfair -> do
-- --         error "future error, unfair"
-- --       WeakFair
-- --         | info ^. nodeNexts == Set.singleton (here ^. worldFingerprint) -> do
-- --           -- Infinite unchanging steps are excluded from eventually under weak fairness.
-- --           pure (there, prevTruth)
-- --         | otherwise -> do
-- --           -- A cyclic behavior of changing steps does not satisfy eventually under weak fairness.
-- --           error "future error, weak fair"
-- --       StrongFair -> strongClosure (view worldFingerprint here) >>= \case
-- --         Nothing -> pure (there, prevTruth)
-- --         Just (Any isStrongFair)
-- --           | isStrongFair || prevTruth -> pure (there, True)
-- --           | otherwise -> error "future error, strong fair"

-- strongClosure :: (MonadState Tabula m, MonadIO m) => Fingerprint -> m (Maybe Any)
-- strongClosure fromFingerprint = do
--   tabula <- get
--   nodeHere <- use (ix fromFingerprint)

--   searchClosure (Set.delete fromFingerprint (view nodeNexts nodeHere))
--     & flip runReaderT (fromFingerprint, Set.empty)
--     & flip evalStateT tabula
--     & runExceptT
--     & fmap (either (const Nothing) Just)
--   where
--     searchClosure ::
--       ( MonadReader (Fingerprint, Set Fingerprint) m
--       , MonadState Tabula m
--       , MonadError () m
--       , MonadIO m
--       ) =>
--       Set Fingerprint ->
--       m Any
--     searchClosure fingerprints
--       | Set.null fingerprints = throwError ()
--       | otherwise = do
--         forAp fingerprints \fingerprint -> do
--           start <- view _1
--           travelled <- view (_2 . to (Set.member fingerprint))

--           if fingerprint == start
--             then pure (Any True)
--             else
--               if travelled
--                 then pure (Any True)
--                 else do
--                   nodeHere <- use (ix fromFingerprint)

--                   liftIO (print fingerprint)
--                   liftIO (print (view nodeNexts nodeHere))
--                   local (second (Set.insert fingerprint)) do
--                     searchClosure (Set.delete fingerprint (view nodeNexts nodeHere))

-- setupTabula :: Set String -> Set String -> Tabula
-- setupTabula props acts =
--   let propBinders = Map.fromList (zip (Set.toList props) [1 ..])
--       actBinders = Map.fromList (zip (Set.toList acts) [1 ..])
--    in Tabula IntMap.empty propBinders actBinders 0
-- {-# INLINE setupTabula #-}
