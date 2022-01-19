{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Model where

import Control.Applicative
import Control.Arrow (returnA)
import Control.Comonad.Cofree
import Control.Comonad.Store
import Control.Monad.Codensity
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Function
import Data.Functor.Identity
import Data.Hashable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Debug.Trace as Debug
import Lens.Micro
import Lens.Micro.Mtl hiding ((.=))

import Control.Applicative.Day
import Control.Applicative.Phases (Phases (Here, There))
import Control.Applicative.Queue
import Control.Comonad
import Control.Monad.Levels
import Data.Bag (Bag (None, Some), zipBagWith)
import qualified Data.Bag as Bag
import Data.Bifunctor
import Data.Context
import Data.Foldable
import Data.Functor.Contravariant
import Data.Functor.Temporal.Basic
import Data.Kind
import Data.Maybe
import Data.Semigroup
import Data.Tree
import Data.Type.Rec hiding (concat)
import Data.World
import Language.Spectacle.AST
import Language.Spectacle.Behavior
import Language.Spectacle.Checker.Fingerprint
import Language.Spectacle.Checker.MCError
import Language.Spectacle.Lang
import Language.Spectacle.Model.Classifier
import Language.Spectacle.Specification
import Language.Spectacle.Syntax
import Language.Spectacle.Syntax.NonDet

-- ---------------------------------------------------------------------------------------------------------------------

initialTimes ::
  (ctxt ~ VariableCtxt vars, Hashable (Rec ctxt), HasVariables vars) =>
  vars ->
  Either [MCError ctxt] (Set (World ctxt))
initialTimes spec = do
  let sets =
        takeInitialActions spec
          & fieldMap (runLang . runNonDetA @[])
          & seqRec
          & foldMap (Set.singleton . makeWorld)

  if Set.null sets
    then throwError [MCNoInitialStatesError]
    else return sets
  where
    seqRec :: RecT [] ctxt -> [Rec ctxt]
    seqRec = \case
      RNilT -> [RNil]
      RConT name states xs -> liftA2 (RCon name) states (seqRec xs)

-- | Setting up necessary action information mapped by the actions name.
--
-- @since 0.1.0.0
setupActionInfo ::
  [(Fairness, String, Action ctxt Bool)] ->
  Map String PropInfo ->
  Map String (Fairness, PropInfo)
setupActionInfo acts propInfos =
  let elements :: [(Fairness, PropInfo)]
      elements = foldMap (\(fair, name, _) -> Debug.trace (show propInfos) [(fair, propInfos Map.! name)]) acts

      keyIdents :: [String]
      keyIdents = foldMap (\(_, name, _) -> [name]) acts
   in Map.fromList (zip keyIdents elements)

-- | Taking the actions from a specification of actions @acts ctxt@.
--
-- @since 0.1.0.0
setupActions ::
  (Hashable (Rec ctxt), HasActions ctxt acts) =>
  acts ctxt ->
  [(Fairness, String, Action ctxt Bool)]
setupActions = reflectActs . takeActionSpine
  where
    reflectActs ::
      Hashable (Rec ctxt) =>
      ActionSpine ctxt acts ->
      [(Fairness, String, Action ctxt Bool)]
    reflectActs = \case
      ActionSpineNil -> []
      ActionSpineCon actDecl acts ->
        let act = case actDecl of
              UnfairAction name f -> (Unfair, show name, f)
              WeakFairAction name f -> (WeakFair, show name, f)
              StrongFairAction name f -> (StrongFair, show name, f)
         in act : reflectActs acts

runModelCheck ::
  forall vars spec prop ctxt acts.
  ( Specification vars spec prop
  , ctxt ~ VariableCtxt vars
  , acts ~ ActionCtxt ctxt spec
  , Show (Rec ctxt)
  , Hashable (Rec ctxt)
  , Ord (Rec ctxt)
  ) =>
  Spec vars spec prop ->
  IO (Either [MCError ctxt] ())
runModelCheck spec@(Spec vs sp) = do
  let actions = setupActions sp
      actInfo = setupActionInfo actions (collectPropInfo @prop)
      tabula = (setupTabula `on` Set.fromList) (map (view _2) actions) (Map.keys actInfo)

  case initialTimes vs of
    Left err -> return (Left err)
    Right initWorlds -> do
      testResult <-
        runCoK (checkLoop 0 actions actInfo initWorlds) (wrap . pure . fst)
          & observeLevelsT
          & flip runStateT tabula

      print "result:"
      print (fst testResult)
      pure (Right ())

-- case initialTimes vs of
--   Left err -> return (Left err)
--   Right initWorlds -> do
--     let [f, g] = map (\(_, _, act) -> act) actions

--         -- canonicalAction :: World ctxt -> Set (World ctxt)
--         -- canonicalAction (World _ vars) = foldMap (runAction vars) acts

--         testS ::
--           (MonadIO m, MonadState Tabula m) =>
--           Set (World ctxt) ->
--           CoK (Set (World ctxt)) m (Set (World ctxt), Bool)
--         testS worlds = do
--           when (Set.null worlds) empty
--           (xs, xHolds) <-
--             globalM
--               (pure . not . Set.null)
--               (foldMap (\x -> runAction (view worldValues x) f) worlds, True)
--           (ys, yHolds) <-
--             futureM
--               (pure . not . Set.null)
--               (foldMap (\x -> runAction (view worldValues x) g) worlds, False)
--           --   >>= \(xs, t) -> do
--           -- let xs' = foldMap (\x -> runAction (view worldValues x) g) xs
--           -- pure (xs', t)

--           zs <- CoK \_ -> handleWorlds (Set.union xs ys) True
--           lift (liftIO (print zs))
--           testS zs

--     testResult <-
--       runCoK (testS initWorlds) (pure . fst)
--         & execLevelsT
--         & flip runStateT (Tabula IntMap.empty Map.empty 0)

--     -- print testResult
--     pure (Right ())

checkLoop ::
  (MonadIO m, MonadState Tabula m, Hashable (Rec ctxt), Show (Rec ctxt)) =>
  Int ->
  [(Fairness, String, Action ctxt Bool)] ->
  Map String (Fairness, PropInfo) ->
  Set (World ctxt) ->
  CoK (Time (World ctxt)) m (Time (World ctxt), Bool)
checkLoop depth actions actInfo worlds
  | Set.null worlds = do
    nexts <- forAp actions \(fair, ident, action) ->
      case snd <$> Map.lookup ident actInfo of
        Nothing -> empty
        Just propInfo ->
          fst <$> makeActionCoK (fair, ident, action) propInfo Infinity
    lift (liftIO (putStrLn ("final: " ++ show depth)))
    empty
  | otherwise = do
    nexts <- forAp actions \(fair, ident, action) ->
      forAp worlds \here -> do
      case snd <$> Map.lookup ident actInfo of
        Nothing -> empty
        Just propInfo -> do
          let nexts = actionLambda action here
          let adjacent = Set.map (view worldFingerprint) (fromTime nexts)

          (nexts', _) <-
            makeActionCoK (fair, ident, action) propInfo nexts >>= \(xs, t) -> do
              let xs' = foldMap (actionLambda action) (fromTime xs)
              actIdx <- lift (gets (ixAction ident))
              pure (xs', t)
          lift (liftIO (putStrLn ident))
          lift (liftIO (print $ here))

          pure [(here, fromTime nexts')]

    unMarked <- filterM (fmap isNothing . lift . use . ixWorld . fst) nexts

    nexts' <- forM unMarked \(here, there) -> do
      let adjacent = Set.map (view worldFingerprint) there
      lift (ixWorld here %= (<> Just (TabulaNode adjacent IntMap.empty)))
      pure there

    lift (liftIO (putStrLn "--"))
    -- lift (liftIO (putStrLn (show ident)))
    -- lift (liftIO (putStrLn ("here: " ++ show worlds)))
    -- lift (liftIO (putStrLn ("there: " ++ show nexts')))
    checkLoop (1 + depth) actions actInfo (Set.unions nexts')

makeActionCoK ::
  (MonadIO m, MonadState Tabula m, Hashable (Rec ctxt), Show (Rec ctxt)) =>
  (Fairness, String, Action ctxt Bool) ->
  PropInfo ->
  Time (World ctxt) ->
  CoK (Time (World ctxt)) m (Time (World ctxt), Bool)
makeActionCoK (fair, ident, act) actInfo worlds
  | propInfoIsAlways actInfo = do
    (worlds', truth) <- globalM (pure . inhabited) (worlds, True)
    return (worlds', truth)
  | propInfoIsEventually actInfo = do
    (worlds', truth) <- futureM (pure . inhabited) (worlds, False)
    return (worlds', truth)
  | propInfoIsInfinitelyOften actInfo =
    undefined
  | propInfoIsStaysAs actInfo =
    undefined
  | not (Set.null (propInfoLeadsTo actInfo)) =
    undefined
  | otherwise =
    undefined

setupTabula :: Set String -> Set String -> Tabula
setupTabula props acts =
  let propBinders = Map.fromList (zip (Set.toList props) [1 ..])
      actBinders = Map.fromList (zip (Set.toList acts) [1 ..])
   in Tabula IntMap.empty propBinders actBinders 0
{-# INLINE setupTabula #-}

inhabited :: Time a -> Bool
inhabited Infinity = False
inhabited (Time xs) = not (Set.null xs)

actionLambda :: Hashable (Rec ctxt) => Action ctxt Bool -> World ctxt -> Time (World ctxt)
actionLambda action here =
  let nexts = runAction (view worldValues here) action
   in Time nexts
{-# INLINE actionLambda #-}

handleWorlds :: (Hashable (Rec ctxt), MonadState Tabula m) => Set (World ctxt) -> LevelsT m (Set (World ctxt))
handleWorlds worlds = do
  pruned <- pruneMarked worlds
  let adjacent = Set.map (view worldFingerprint) worlds
  undefined

handleWorld :: (Hashable (Rec ctxt), MonadState Tabula m) => World ctxt -> m (Maybe (World ctxt))
handleWorld world = do
  isMarked <- isJust <$> use (ixWorld world)

  undefined

-- forAp worlds \world -> do
--   let truths = IntMap.singleton 0 True

--   isMarked <- isJust <$> use (ixWorld world)

--   if isMarked
--     then pure Set.empty
--     else do
--       ixWorld world %= (<> Just (TabulaNode adjacent truths))
--       pure (Set.singleton world)

pruneMarked :: (MonadState Tabula m) => Set (World ctxt) -> m (Set (World ctxt))
pruneMarked =
  let go w ws idx
        | isJust idx = ws
        | otherwise = Set.insert w ws
   in Set.foldr (\w ws -> liftA2 (go w) ws (use (ixWorld w))) (pure Set.empty)

-- let actions = setupActions sp
--     actInfo = setupActionInfo actions (collectPropInfo @prop)
-- reactive = setupRSet (map (\(_, name, act) -> (name, act)) actions)

-- case initialTimes vs of
--   Left err -> return (Left err)
--   Right initialWorlds -> do
--     let stream =
--           streamBehaviors @_ @(Except [MCError ctxt]) reactive (head (Set.toList initialWorlds))
--             & pruneCycles
--             & ( \w -> getG (semGlobal "incX" w) <> getG (semGlobal "incY" w))
--             & runQueue
--             & fromLogicT
--             & (>>= fromSearchSpace 0)
--             & observeLevelsT
--             & runExcept
--     -- & (>>= fromSearchSpace 0)
--     -- & runExcept
--     case stream of
--       Left err -> do
--         print "error:"
--         print err
--       Right xs -> do
--         print "success, no output set"
--         -- pure ()
--         mapM_ print xs
--     pure (Right ())
-- where
--   goCofree :: Show a => Int -> Cofree (LogicT IO) a -> IO ()
--   goCofree depth (here :< there) = do
--     putStrLn (show depth ++ ": " ++ show here)
--     observeAllT there >>= mapM_ (goCofree (depth + 1))

-- semGlobal propIdx \nt p -> loop nt p True initWorld

globalS ::
  forall m a.
  (MonadIO m, Show a) =>
  (Set a -> m Bool) ->
  Set a ->
  CoK (Set a) m (Set a, Bool)
globalS p here = CoK \nextS -> do
  let nextK :: Set a -> Bool -> CoK Bool m (Set a)
      nextK xs holdsNow
        | Set.null xs = CoK \_ -> nextS (xs, holdsNow)
        | otherwise = CoK \predicate -> do
          later <- nextS (xs, holdsNow)
          runCoK (globalK later nextK) predicate

  xs <- runCoK (nextK here True) (lift . p)
  pure (xs, True)

globalK ::
  Monad m =>
  Set a ->
  (Set a -> Bool -> CoK Bool m (Set a)) ->
  CoK Bool m (Set a)
globalK here nextK = CoK \predicate -> do
  holds <- predicate here
  if holds
    then pure here <|> runCoK (nextK here True) predicate
    else runCoK (nextK here False) (pure . const False)

-- ---------------------------------------------------------------------------------------------------------------------

type TestSpec =
  Spec
    (Var "x" Int :. Var "y" Int) -- :. Var "z" Int)
    ("incX" !> 'Unfair \/ "incY" !> 'Unfair) -- \/ "incZ" !> 'Unfair)
    (Always "incX" /\ Eventually "incY") -- /\ Eventually (Always "incY"))

incX :: Action (VariableCtxt TestSpec) Bool
incX = do
  x <- plain #x
  if x < 7
    then do
      n <- oneOf [1 .. 2]
      #x .= pure (n + x)
    else #x .= pure 0
  return True

incY :: Action (VariableCtxt TestSpec) Bool
incY = do
  x <- plain #x
  y <- plain #y
  #y .= pure (x + y `mod` 5)
  return True --  (x == 5 || x == 6)

-- incY :: Action (VariableCtxt TestSpec) Bool
-- incY = do
--   x <- plain #x
--   y <- plain #y
--   if y == 0
--     then do
--       #y .= pure 1
--     else do

--       -- nil <- oneOf []
--       #y .= pure 0
--   return True

spec :: TestSpec
spec =
  Spec
    (#x := pure 0 :. #y := pure 0) -- :. #z := pure 0)
    (UnfairAction #incX incX :\/: UnfairAction #incY incY) -- :\/: UnfairAction #incZ incZ)

testSpec :: IO ()
testSpec =
  runModelCheck spec >>= \case
    Left errs -> mapM_ print errs
    Right xs -> print xs -- xs
