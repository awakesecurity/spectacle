{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Language.Spectacle.Checker.Model
  ( -- * Model Operations
    modelNextSets,
    modelFairScheduling,
  )
where

import Control.Monad.Except
import Data.Hashable
import Data.Kind
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Context
import Data.Type.Rec
import Data.World
import Language.Spectacle.Checker.Fingerprint
import Language.Spectacle.Checker.MCError
import Language.Spectacle.Specification.Action

-- ---------------------------------------------------------------------------------------------------------------------
-- Model Operations

modelNextSets ::
  forall (m :: Context -> [Type] -> Type -> Type) (ctxt :: Context) (acts :: [Type]).
  (MonadError [MCError ctxt] (m ctxt acts), Hashable (Rec ctxt)) =>
  World ctxt ->
  ActionSpine ctxt acts ->
  m ctxt acts [ActionSet ctxt]
modelNextSets worldHere spine =
  case spineToActionSets worldHere spine of
    Left err -> throwError [MCActionError worldHere err]
    Right results -> return results
{-# INLINE modelNextSets #-}

modelFairScheduling :: Map String ActionInfo -> Set (String, Set Fingerprint) -> Set (String, Set Fingerprint)
modelFairScheduling actionInfos = foldMap \(action, nexts) ->
  let fairness = maybe Unfair actionInfoFairness (Map.lookup action actionInfos)
   in if fairness == Unfair
        then Set.empty
        else Set.singleton (action, nexts)
{-# INLINE modelFairScheduling #-}



-- stepModelNextSets ::
--   forall ctxt acts.
--   (Show (Rec ctxt), Hashable (Rec ctxt)) =>
--   World ctxt ->
--   Model ctxt acts [ActionSet ctxt]
-- stepModelNextSets worldHere = do
--   actionSets <- sequence . foldActionCtxt <$> view mcEnvActions

--   case actionSets of
--     Left err -> throwError [err]
--     Right results -> forM results \actionSet ->
--       if Set.null (actionStates actionSet)
--         then do
--           actionPropInfo <- Map.lookup (actionName actionSet) <$> view mcEnvPropInfo

--           if maybe False propInfoIsAlways actionPropInfo
--             then throwError [MCImpasseError worldHere]
--             else return actionSet
--         else return actionSet
--   where
--     foldActionCtxt ::
--       ActionCtxt ctxt acts ->
--       [Either (MCError ctxt) (ActionSet ctxt)]
--     foldActionCtxt = \case
--       ActionCtxtNil -> []
--       ActionCtxtCon anAct ctxt -> evalAnAction anAct : foldActionCtxt ctxt

--     evalAnAction ::
--       AnAction ctxt acts' ->
--       Either (MCError ctxt) (ActionSet ctxt)
--     evalAnAction = \case
--       ActionHere _ act -> actionQuotient worldHere act
--       ActionThere actions -> evalAnAction actions

-- stepInitial ::
--   (Show (Rec ctx), Hashable (Rec ctx)) =>
--   World ctx ->
--   Model ctx (Set (World ctx))
-- stepInitial = stepModel 0 . Set.singleton

-- stepModel ::
--   (Show (Rec ctx), Hashable (Rec ctx)) =>
--   Int ->
--   Set (World ctx) ->
--   Model ctx (Set (World ctx))
-- stepModel depth worlds = do
--   nextWorlds <- foldr (liftA2 (<>) . stepNextModel (depth + 1)) (pure Set.empty) worlds

--   if Set.null nextWorlds
--     then empty
--     else pure worlds <|> stepModel (depth + 1) nextWorlds

-- stepNextModel ::
--   (Show (Rec ctx), Hashable (Rec ctx)) =>
--   Int ->
--   World ctx ->
--   Model ctx (Set (World ctx))
-- stepNextModel depth worldHere@(World fingerprint _) = do
--   trace ("step here: " ++ show worldHere) (pure ())
--   trace ("step depth: " ++ show depth) (pure ())

--   explored <- isExploredFingerprint fingerprint

--   view modelFairness >>= \case
--     Unfair
--       | explored -> stopModel depth
--       | otherwise -> error "unfair process: unexplored!"
--     WeakFair
--       | explored -> do
--         allProps <- view livenessPropertyNames
--         satProps <- use (worldCoverage . coverageInfo fingerprint . livenessProperties)
--         let unsatProps = allProps `IntSet.difference` satProps

--         trace (show worldHere ++ " sats " ++ show satProps) (pure ())

--         if IntSet.null unsatProps
--           then stopModel depth
--           else weakLiveness unsatProps depth worldHere
--       | otherwise -> do
--         worldsThere <- takeAction worldHere
--         forM_ worldsThere \worldThere -> do
--           let step = makeStep worldHere worldThere
--           checkFormula step
--         worldCoverage . coverageInfo fingerprint . hasBeenExplored .= True
--         return worldsThere
--     StrongFair
--       | explored -> error "strong fair: explored!"
--       | otherwise -> error "strong fair: unexplored!"

-- stepNextUnique :: (Show (Rec ctx), Hashable (Rec ctx)) => World ctx -> Model ctx ()
-- stepNextUnique worldHere@(World fingerprint _) = do
--   guard . not =<< isExploredWorld worldHere
--   worldCoverage . coverageInfo fingerprint . hasBeenExplored .= True

-- stopModel :: Int -> Model ctx (Set a)
-- stopModel depth = do
--   modelDepth %= max depth
--   return Set.empty

-- weakLiveness ::
--   (Hashable (Rec ctx), Show (Rec ctx)) =>
--   IntSet ->
--   Int ->
--   World ctx ->
--   Model ctx (Set (World ctx))
-- weakLiveness props depth worldHere@(World fingerprint _) = do
--   enabledNexts <- takeEnabledNexts fingerprint

--   trace ("world: " ++ show worldHere) (pure ())

--   if Set.null enabledNexts
--     then do
--       -- stopModel depth
--       error "no enabled nexts"
--       -- trace ("marking out world: " ++ show worldHere) (pure ())
--       -- cycles <- takeCyclesFrom fingerprint <$> use worldCoverage

--       -- forM_ cycles \case
--       --   (DeadEnd _, _) -> return ()
--       --   (CompleteCycle _, xs) -> disableCycle fingerprint xs

--       -- stopModel depth
--     else do
--       cycles <- takeCyclesFrom fingerprint <$> use worldCoverage
--       worldsThere <- takeAction worldHere

--       foreach worldsThere \worldThere@(World there _) -> do
--         if Set.member there enabledNexts
--           then do
--             let loops = takeClosedCycles there cycles

--             forM_ loops \case
--               (DeadEnd _, _) -> return ()
--               (ClosedCycle there', loop) -> do
--                 disableCycle fingerprint loop
--                 worldCoverage . coverageInfo fingerprint . disabledNextWorlds %= Set.insert there'

--             -- error (show cycles)
--             return (Set.singleton worldThere)
--           else return Set.empty

-- takeEnabledNexts :: Fingerprint -> Model ctx (Set Fingerprint)
-- takeEnabledNexts fingerprint = do
--   nexts <- use (worldCoverage . coverageInfo fingerprint . nextWorlds)
--   disabledNexts <- use (worldCoverage . coverageInfo fingerprint . disabledNextWorlds)
--   return (nexts `Set.difference` disabledNexts)

-- isDisabledWorld ::  Fingerprint -> Model ctx Bool
-- isDisabledWorld fingerprint = do
--   nexts <- use (worldCoverage . coverageInfo fingerprint . nextWorlds)
--   disabled <- use (worldCoverage . coverageInfo fingerprint . disabledNextWorlds)
--   return (nexts == disabled)

-- disableCycle :: Fingerprint -> [Fingerprint] -> Model ctx ()
-- disableCycle _ [] = return ()
-- disableCycle here (there : xs) = do
--   worldCoverage . coverageInfo here . disabledNextWorlds %= Set.insert there
--   propagateLiveness (StepImage here there)
--   disableCycle there xs

-- takeAction ::
--   (Hashable (Rec ctx)) =>
--   World ctx ->
--   Model ctx (Set (World ctx))
-- takeAction worldHere@(World fingerprint _) = do
--   undefined
--   -- worlds <- actionQuotient worldHere <$> view modelAction
--   -- case worlds of
--   --   Left mcerr -> throwError [mcerr]
--   --   Right (ActionSet name worldsThere) -> do
--   --     worldCoverage . coverageInfo fingerprint . nextWorlds .= Set.map (^. worldFingerprint) worldsThere
--   --     return worldsThere

-- takeFormulaTerms :: Step ctx -> Model ctx (Term Bool)
-- takeFormulaTerms step@(Step worldHere worldThere) = do
--   formula <- view modelFormula
--   case runInvariant True (worldHere ^. worldValues) (worldThere ^. worldValues) formula of
--     Left err -> throwError [MCFormulaRuntimeError step err]
--     Right terms -> return terms

-- -- | Checks if extending the current execution trace with an infinite sequence of stutter-step violates the temporal
-- -- formula.
-- --
-- -- @since 0.1.0.0
-- checkInfiniteReflexStep :: World ctx -> Model ctx ()
-- checkInfiniteReflexStep world@(World fingerprint _) = do
--   let step = makeReflexStep world

--   checkFormula step

--   -- check if all liveness properties have been satisfies here. for an infinite w --> w, if there is some liveness
--   -- property which has not yet been satisfied, then it will never be satisfied.
--   allProps <- view livenessPropertyNames
--   satProps <- use (worldCoverage . coverageInfo fingerprint . livenessProperties)
--   let unsatProps = allProps `IntSet.difference` satProps

--   unless (IntSet.null unsatProps) do
--     errs <- forM (IntSet.toList unsatProps) \prop -> do
--       srcLoc <- IntMap.lookup prop <$> view srcLocMap
--       return (MCFormulaError step (join srcLoc) EventuallyPropK)

--     throwError errs

-- -- | @'checkFormula' step@ checks @step@ satisfies the models temporal formula, throwing a model checker error if the
-- -- formula is violated.
-- --
-- -- @since 0.1.0.0
-- checkFormula :: Step ctx -> Model ctx ()
-- checkFormula step@(Step worldHere _) = do
--   fairness <- view modelFairness

--   case fairness of
--     Unfair -> do
--       void . checkTerms step =<< takeFormulaTerms step
--       checkInfiniteReflexStep worldHere
--       propagateLiveness (toStepImage step)
--     WeakFair -> do
--       let stutter = makeReflexStep worldHere

--       void . checkTerms step =<< takeFormulaTerms step
--       void . checkTerms stutter =<< takeFormulaTerms stutter

--       propagateLiveness (toStepImage step)
--     StrongFair -> do
--       let stutter = makeReflexStep worldHere
--       void . checkTerms step =<< takeFormulaTerms step
--       void . checkTerms stutter =<< takeFormulaTerms stutter
--       propagateLiveness (toStepImage step)

-- checkTerms :: Step ctx -> Term Bool -> Model ctx Bool
-- checkTerms step = \case
--   Value x -> return x
--   Conjunct e1 e2 -> liftA2 (&&) (checkTerms step e1) (checkTerms step e2)
--   Disjunct e1 e2 ->
--     view disjunctQueue >>= \case
--       [] -> throwError [MCInternalError EmptyDisjunctQueueK]
--       LeftBranch : xs -> local (set disjunctQueue xs) (checkTerms step e1)
--       RightBranch : xs -> local (set disjunctQueue xs) (checkTerms step e2)
--   Complement e -> not <$> checkTerms step e
--   Always srcLoc name e -> checkAlways step name srcLoc e
--   Eventually _ name e -> checkEventually step name e
--   UpUntil srcLoc name e1 e2 -> checkUpUntil step name srcLoc e1 e2
--   StaysAs _ name e -> do
--     result <- checkTerms step e
--     truthCoverage . stepTruth step name .= result
--     return result
--   InfinitelyOften _ name e -> do
--     result <- checkTerms step e
--     truthCoverage . stepTruth step name .= result
--     return result

-- -- | @'checkAlways' step name srcLoc e@ checks a formula @always e@ identified by @name@ for the model-step @step@.
-- --
-- -- @since 0.1.0.0
-- checkAlways :: forall ctx. Step ctx -> Int -> Maybe SrcLoc -> Term Bool -> Model ctx Bool
-- checkAlways step name srcLoc term = do
--   result <- checkTerms step term
--   truthCoverage . stepTruth step name .= result
--   if result
--     then return result
--     else throwError [MCFormulaError step srcLoc AlwaysPropK]
-- {-# INLINE checkAlways #-}

-- -- | @'checkEventually' step name e@ checks a formula @eventually e@ identified by @name@ for the model-step @step@.
-- --
-- -- @since 0.1.0.0
-- checkEventually :: Step ctx -> Int -> Term Bool -> Model ctx Bool
-- checkEventually step@(Step worldHere worldThere) name e = do
--   result <- checkTerms step e
--   truthCoverage . stepTruth step name .= result

--   when result do
--     worldCoverage . coverageInfo (worldHere ^. worldFingerprint) . livenessProperties %= IntSet.insert name
--     worldCoverage . coverageInfo (worldThere ^. worldFingerprint) . livenessProperties %= IntSet.insert name

--   return True
-- {-# INLINE checkEventually #-}

-- checkUpUntil :: Step ctx -> Int -> Maybe SrcLoc -> Term Bool -> Term Bool -> Model ctx Bool
-- checkUpUntil step@(Step worldHere worldThere) name srcLoc e1 e2 = do
--   result1 <- checkTerms step e1
--   result2 <- checkTerms step e2
--   truthCoverage . stepTruth step name .= result2

--   unless (result1 || result2) (throwError [MCFormulaError step srcLoc UpUntilPropK])

--   when result2 do
--     worldCoverage . coverageInfo (worldHere ^. worldFingerprint) . livenessProperties %= IntSet.insert name
--     worldCoverage . coverageInfo (worldThere ^. worldFingerprint) . livenessProperties %= IntSet.insert name

--   return (result1 || result2)
-- {-# INLINE checkUpUntil #-}

-- -- | If @s@ is the given 'StepImage', @'propagateLiveness' s@ updates the coverage information such that all liveness
-- -- properties satisfied by the initial world in step are also satisfied by the terminal world in the step. The
-- -- implication being if @eventually p@ is true for a world @w_n@, then it follows that @eventually p@ is also true for
-- -- a world @w_n+1@.
-- --
-- -- @since 0.1.0.0
-- propagateLiveness :: StepImage -> Model ctx ()
-- propagateLiveness step@(StepImage fromWorld toWorld) = do
--   satProps <- use (worldCoverage . coverageInfo fromWorld . livenessProperties)

--   worldCoverage . coverageInfo toWorld . livenessProperties .= satProps
--   truthCoverage . stepImageTruthTable step .= TruthTable (IntSet.foldr (`IntMap.insert` True) IntMap.empty satProps)

-- -- | A predicate for if the given world has been explored by the model checker.
-- --
-- -- @since 0.1.0.0
-- isExploredWorld :: World ctx -> Model ctx Bool
-- isExploredWorld world = isExploredFingerprint (world ^. worldFingerprint)
-- {-# INLINE isExploredWorld #-}

-- -- | A predicate for if the given world has not yet been explored.
-- --
-- -- @since 0.1.0.0
-- isUnexploredWorld :: World ctx -> Model ctx Bool
-- isUnexploredWorld = fmap not . isExploredWorld
-- {-# INLINE isUnexploredWorld #-}

-- -- | A predicate for if the given fingerprint has been explored.
-- --
-- -- @since 0.1.0.0
-- isExploredFingerprint :: Fingerprint -> Model ctx Bool
-- isExploredFingerprint fingerprint = use (worldCoverage . coverageInfo fingerprint . hasBeenExplored)
-- {-# INLINE isExploredFingerprint #-}

-- -- | A predicate for if the given fingerprint has not yet been checked.
-- --
-- -- @since 0.1.0.0
-- isUnexploredFingerprint :: Fingerprint -> Model ctx Bool
-- isUnexploredFingerprint = fmap not . isExploredFingerprint
-- {-# INLINE isUnexploredFingerprint #-}

{-

-- | 'stepModel' is the main model checker loop.
--
-- @since 0.1.0.0
stepModel :: (Show (Rec ctx), Hashable (Rec ctx)) => World ctx -> Model ctx (World ctx)
stepModel worldHere = do
  step <- nextStep worldHere
  fairness <- view modelFairness

  worldThere <- case fairness of
    Unfair -> unfairStep step
    WeakFair -> weakFairStep step
    StrongFair -> strongFairStep step

  worldCoverage . coverageInfo (worldHere ^. worldFingerprint) . hasBeenExplored .= True

  canTerminate <- checkTermination True worldThere
  if canTerminate
    then return worldThere
    else stepModel worldThere

-- | Takes a single model step for a unfair process.
--
-- @since 0.1.0.0
unfairStep :: Step ctx -> Model ctx (World ctx)
unfairStep step@(Step worldHere worldThere) = do
  checkFormula step
  checkInfiniteStutter worldHere

  liveness <- weakLivenessCheck (worldThere ^. worldFingerprint)
  unless liveness (throwError =<< cyclicLivenessErrors step)

  return worldThere
{-# INLINE unfairStep #-}

-- | Takes a single model step under a weak fairness constraint.
--
-- @since 0.1.0.0
weakFairStep :: Step ctx -> Model ctx (World ctx)
weakFairStep step@(Step worldHere worldThere) = do
  checkFormula step
  checkStutter worldHere

  liveness <- weakLivenessCheck (worldThere ^. worldFingerprint)
  unless liveness (throwError =<< cyclicLivenessErrors step)

  return worldThere
{-# INLINE weakFairStep #-}

-- | Takes a single model step under a strong fairness constraint.
--
-- @since 0.1.0.0
strongFairStep :: Step ctx -> Model ctx (World ctx)
strongFairStep step = do
  checkFormula step
  checkStutter (step ^. stepFrom)
  return (step ^. stepTo)
{-# INLINE strongFairStep #-}

-- | Generates a model step to check from the world given.
--
-- @since 0.1.0.0
nextStep :: Hashable (Rec ctx) => World ctx -> Model ctx (Step ctx)
nextStep world =
  takeQuotientOf world >>= foldMapA \world' ->
    if world == world'
      then do
        let step = makeStep world world'
        fairness <- view modelFairness

        _ <- case fairness of
          Unfair -> unfairStep step
          WeakFair -> weakFairStep step
          StrongFair -> strongFairStep step

        errs <- cyclicLivenessErrors step
        guard (not (null errs))
        throwError errs
      else return (makeStep world world')
{-# INLINE nextStep #-}

-- ---------------------------------------------------------------------------------------------------------------------
-- Model Checking Operations

-- | Checks if extending the current execution trace with some finite number of stutter-steps violates the temporal
-- formula.
--
-- @since 0.1.0.0
checkStutter :: World ctx -> Model ctx ()
checkStutter world = do
  result <- (checkFormula (makeReflexStep world) >> pure Nothing) `catchError` (pure . Just)
  case result of
    Nothing -> return ()
    Just errs -> do
      let annotated = map (stutterErrorAnnotation FiniteStutterK) errs
      throwError annotated
{-# INLINE checkStutter #-}

-- | Checks if extending the current execution trace with an infinite sequence of stutter-step violates the temporal
-- formula.
--
-- @since 0.1.0.0
checkInfiniteStutter :: World ctx -> Model ctx ()
checkInfiniteStutter world = do
  let stutter = makeReflexStep world
  checkFormula stutter
  satLiveness <- satisfiesLiveness (world ^. worldFingerprint)
  unless satLiveness do
    errors <- cyclicLivenessErrors stutter
    throwError (map (stutterErrorAnnotation InfiniteStutterK) errors)
{-# INLINE checkInfiniteStutter #-}

-- | Checks if a world meets the termination condition. If a termination condition was not specified then
-- 'checkTermination' is always 'False'.
--
-- @since 0.1.0.0
checkTermination :: Bool -> World ctx -> Model ctx Bool
checkTermination isEnabled (World fingerprint world) =
  view modelTerminate >>= \case
    Nothing -> return False
    Just terminate -> do
      satLiveness <- satisfiesLiveness fingerprint
      if satLiveness
        then return (runTerminate isEnabled world terminate)
        else return False
{-# INLINE checkTermination #-}

checkFormula :: Step ctx -> Model ctx ()
checkFormula step = do
  terms <- runFormulaTerms step
  void (checkTerms step terms)
  propagateLiveness (toStepImage step)
  return ()
{-# INLINE checkFormula #-}

-- | Produces the 'Term' tree of the models temporal formula.
--
-- @since 0.1.0.0
runFormulaTerms :: Step ctx -> Model ctx (Term Bool)
runFormulaTerms step@(Step worldHere worldThere) = do
  formula <- view modelFormula
  case runInvariant True (worldHere ^. worldValues) (worldThere ^. worldValues) formula of
    Left exc -> throwError [MCFormulaRuntimeError step exc]
    Right terms -> return terms
{-# INLINE runFormulaTerms #-}

-- | 'checkTerms' evaluates a 'Term' tree of the model's temporal formula at the given step to verify that the
-- step does not violate the properties specified.
--
-- @since 0.1.0.0
checkTerms :: Step ctx -> Term Bool -> Model ctx Bool
checkTerms step = \case
  Value x -> return x
  Conjunct e1 e2 -> do
    result1 <- checkTerms step e1
    result2 <- checkTerms step e2
    return (result1 && result2)
  Disjunct e1 e2 ->
    view disjunctQueue >>= \case
      [] -> throwError [MCInternalError EmptyDisjunctQueueK]
      LeftBranch : xs -> local (set disjunctQueue xs) (checkTerms step e1)
      RightBranch : xs -> local (set disjunctQueue xs) (checkTerms step e2)
  Complement e -> not <$> checkTerms step e
  Always srcLoc name e -> checkAlways step name srcLoc e
  Eventually _ name e -> checkEventually step name e
  UpUntil srcLoc name e1 e2 -> checkUpUntil step name srcLoc e1 e2
  StaysAs _ name e -> do
    result <- checkTerms step e
    truthCoverage . stepTruth step name .= result
    return result
  InfinitelyOften _ name e -> do
    result <- checkTerms step e
    truthCoverage . stepTruth step name .= result
    return result
{-# INLINE checkTerms #-}

-- | @'checkAlways' step name srcLoc e@ checks a formula @always e@ identified by @name@ for the model-step @step@.
--
-- @since 0.1.0.0
checkAlways :: forall ctx. Step ctx -> Int -> Maybe SrcLoc -> Term Bool -> Model ctx Bool
checkAlways step name srcLoc e = do
  result <- checkTerms step e
  truthCoverage . stepTruth step name .= result
  if result
    then return result
    else throwError [MCFormulaError step srcLoc AlwaysPropK]
{-# INLINE checkAlways #-}

-- | @'checkEventually' step name e@ checks a formula @eventually e@ identified by @name@ for the model-step @step@.
--
-- @since 0.1.0.0
checkEventually :: Step ctx -> Int -> Term Bool -> Model ctx Bool
checkEventually step@(Step worldHere worldThere) name e = do
  result <- checkTerms step e
  truthCoverage . stepTruth step name .= result

  when result do
    worldCoverage . coverageInfo (worldHere ^. worldFingerprint) . livenessProperties %= IntSet.insert name
    worldCoverage . coverageInfo (worldThere ^. worldFingerprint) . livenessProperties %= IntSet.insert name

  return True
{-# INLINE checkEventually #-}

-- | @'checkUpUntil' step name srcLoc e1 e2@ checks a formula @upUntil e1 e2@ identified by @name@ for the model-step
-- @step@.
--
-- @since 0.1.0.0
checkUpUntil :: Step ctx -> Int -> Maybe SrcLoc -> Term Bool -> Term Bool -> Model ctx Bool
checkUpUntil step@(Step worldHere worldThere) name srcLoc e1 e2 = do
  result1 <- checkTerms step e1
  result2 <- checkTerms step e2
  truthCoverage . stepTruth step name .= result2

  unless (result1 || result2) (throwError [MCFormulaError step srcLoc UpUntilPropK])

  when result2 do
    worldCoverage . coverageInfo (worldHere ^. worldFingerprint) . livenessProperties %= IntSet.insert name
    worldCoverage . coverageInfo (worldThere ^. worldFingerprint) . livenessProperties %= IntSet.insert name

  return (result1 || result2)
{-# INLINE checkUpUntil #-}

-- | Preforms a liveness check under a weak-fairness constraint.
--
-- @since 0.1.0.0
weakLivenessCheck :: Fingerprint -> Model ctx Bool
weakLivenessCheck fingerprint = do
  explored <- isExploredFingerprint fingerprint
  if explored
    then satisfiesLiveness fingerprint
    else return True
{-# INLINE weakLivenessCheck #-}

-- ---------------------------------------------------------------------------------------------------------------------
-- Model Predicates

-- | A predicate for if the given world has been explored by the model checker.
--
-- @since 0.1.0.0
isExploredWorld :: World ctx -> Model ctx Bool
isExploredWorld world = isExploredFingerprint (world ^. worldFingerprint)
{-# INLINE isExploredWorld #-}

-- | A predicate for if the given world has not yet been explored.
--
-- @since 0.1.0.0
isUnexploredWorld :: World ctx -> Model ctx Bool
isUnexploredWorld = fmap not . isExploredWorld
{-# INLINE isUnexploredWorld #-}

-- | A predicate for if the given fingerprint has been explored.
--
-- @since 0.1.0.0
isExploredFingerprint :: Fingerprint -> Model ctx Bool
isExploredFingerprint fingerprint = use (worldCoverage . coverageInfo fingerprint . hasBeenExplored)
{-# INLINE isExploredFingerprint #-}

-- | A predicate for if the given fingerprint has not yet been checked.
--
-- @since 0.1.0.0
isUnexploredFingerprint :: Fingerprint -> Model ctx Bool
isUnexploredFingerprint = fmap not . isExploredFingerprint
{-# INLINE isUnexploredFingerprint #-}

-- | A predicate for if all liveness properties have been satisfied so far for the given fingerprint.
--
-- @since 0.1.0.0
satisfiesLiveness :: Fingerprint -> Model ctx Bool
satisfiesLiveness fingerprint = do
  allLivenessProps <- view livenessPropertyNames
  satLivenessProps <- use (worldCoverage . coverageInfo fingerprint . livenessProperties)
  return (allLivenessProps == satLivenessProps)
{-# INLINE satisfiesLiveness #-}

-- ---------------------------------------------------------------------------------------------------------------------
-- World Operations

-- | @'takeQuotientOf' w@ for some world @w@ evaluates the next-state relation at @w@. Producing either a set of new
-- states related by the action to explore or exceptions if the model is at an impasse.
--
-- @since 0.1.0.0
takeQuotientOf :: forall ctx. Hashable (Rec ctx) => World ctx -> Model ctx (Set (World ctx))
takeQuotientOf world@(World fingerprint values) = do
  results <- actionQuotient world <$> view modelAction
  case result of
    Left mcerr -> throwError [mcerr]
    Right worlds -> _

  -- case results of
  --   Left exc -> throwError [MCActionError world exc]
  --   Right
  --     | null results -> throwError [MCImpasseError world]
  --     | otherwise -> do
  --       let newWorlds = foldMap (uncurry dropUnrelated . first newValues) results
  --       isUnexplored <- isUnexploredFingerprint fingerprint

  --       if isUnexplored
  --         then do
  --           worldCoverage . coverageInfo fingerprint . succeedingWorlds .= Set.map (^. worldFingerprint) newWorlds
  --           modelDepth += 1
  --           return newWorlds
  --         else do
  --           step <- foldMapA (pure . makeStep world) newWorlds
  --           fairness <- view modelFairness

  --           _ <- case fairness of
  --             Unfair -> unfairStep step
  --             WeakFair -> weakFairStep step
  --             StrongFair -> strongFairStep step

  --           errs <- cyclicLivenessErrors step
  --           guard (not (null errs))
  --           throwError errs
  -- where
  --   dropUnrelated :: Rec ctx -> Bool -> Set (World ctx)
  --   dropUnrelated world' isRelated
  --     | isRelated = Set.singleton (newWorld world')
  --     | otherwise = Set.empty
{-# INLINE takeQuotientOf #-}

-- | If @s@ is the given 'StepImage', @'propagateLiveness' s@ updates the coverage information such that all liveness
-- properties satisfied by the initial world in step are also satisfied by the terminal world in the step. The
-- implication being if @eventually p@ is true for a world @w_n@, then it follows that @eventually p@ is also true for
-- a world @w_n+1@.
--
-- @since 0.1.0.0
propagateLiveness :: StepImage -> Model ctx ()
propagateLiveness step@(StepImage fromWorld toWorld) = do
  worldCoverage . coverageInfo toWorld . livenessProperties
    <~ use (worldCoverage . coverageInfo fromWorld . livenessProperties)

  truthCoverage . stepImageTruthTable step <~ do
    livenessProps <- use (worldCoverage . coverageInfo fromWorld . livenessProperties)
    let table = IntSet.foldr (`IntMap.insert` True) IntMap.empty livenessProps
    return (TruthTable table)
{-# INLINE propagateLiveness #-}

-- | Retrieves a set of liveness properties that have not been satisfied by the given step.
--
-- @since 0.1.0.0
getUnsatisfiedLiveness :: Step ctx -> Model ctx [Int]
getUnsatisfiedLiveness step = do
  allProps <- view livenessPropertyNames
  nub . concat <$> forM (IntSet.toList allProps) \name -> do
    truth <- use (truthCoverage . stepTruth step name)
    if truth
      then return []
      else return [name]
{-# INLINE getUnsatisfiedLiveness #-}

-- ---------------------------------------------------------------------------------------------------------------------
-- Model Errors

-- | Annotate a 'MCFormulaError' as a formula violation that occured for a stuttering step.
--
-- @since 0.1.0.0
stutterErrorAnnotation :: StutterKind -> MCError ctx -> MCError ctx
stutterErrorAnnotation stutterK = \case
  MCFormulaError step srcLoc propK -> MCStutterError step srcLoc propK stutterK
  mcError -> mcError
{-# INLINE stutterErrorAnnotation #-}

-- | Produce errors for all liveness properties that are unsatisfied within some transitively closed cycle of a models
-- state graph.
--
-- @since 0.1.0.0
cyclicLivenessErrors :: forall ctx. Step ctx -> Model ctx [MCError ctx]
cyclicLivenessErrors step = do
  unsatProps <- getUnsatisfiedLiveness step
  forM unsatProps \prop -> do
    srcLoc <- view (srcLocMap . to (join . IntMap.lookup prop))
    return (MCFormulaError step srcLoc EventuallyPropK)
{-# INLINE cyclicLivenessErrors #-}

-}
