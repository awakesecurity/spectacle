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
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.State.Strict
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
import Control.Monad.Codensity
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
  ( ctxt ~ VariableCtxt vars
  , Hashable (Rec ctxt)
  , HasVariables vars
  ) =>
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
      elements = foldMap (\(fair, name, _) -> [(fair, propInfos Map.! name)]) acts

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

-- setupRSet ::
--   Hashable (Rec ctxt) =>
--   [(String, Action ctxt Bool)] ->
--   RSet (World ctxt) (Set (Interval (World ctxt) (Set (World ctxt))))
-- setupRSet = spanRSets . map (uncurry fromAction)

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

  case initialTimes vs of
    Left err -> return (Left err)
    Right initWorlds -> do
      let acts :: [Action ctxt Bool]
          acts = map (\(_, _, act) -> act) actions

          canonicalAction :: World ctxt -> Set (World ctxt)
          canonicalAction (World _ vars) = foldMap (runAction vars) acts

          testSearch :: MonadIO f => LevelsT f (Bool, World ctxt)
          testSearch = do
            initWorld <- sumFoldable initWorlds
            pure (True, initWorld) <|> globalF 0 initWorld canonicalAction

      testResult <-
        testSearch
          & observeLevelsT
      pure (Right ())

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
globalF ::
  forall f ctxt.
  (MonadIO f, Show (Rec ctxt)) =>
  Int ->
  World ctxt ->
  (World ctxt -> Set (World ctxt)) ->
  LevelsT f (Bool, World ctxt)
globalF propIdx initWorld act = derived \nt p -> loop nt p True initWorld
  where
    derived ::
      (forall g. Monad g => (LevelsT f ~> g) -> (World ctxt -> g Bool) -> g (World ctxt)) ->
      LevelsT f (Bool, World ctxt)
    derived phi =
      runK
        (phi lift (semGlobal propIdx))
        predicate
        (Tabula IntMap.empty Map.empty 0)

    loop ::
      Monad g =>
      (LevelsT f ~> g) ->
      (World ctxt -> g Bool) ->
      Bool ->
      World ctxt ->
      g (World ctxt)
    loop nt p propHolds world
      | propHolds = do
        holdsNext <- p world
        nextWorld <- nt (sumFoldable (act world))

        loop nt p holdsNext nextWorld
      | otherwise = undefined -- TODO

    predicate :: World ctxt -> Tabula -> LevelsT f (Bool, World ctxt)
    predicate world tabula = case view (ixWorld world) tabula of
      Nothing -> undefined -- TODO: error? ixWorld should always exist in the tabula by the point the predicate is applied
      Just node -> pure (view (ixProp propIdx) node, world)

semGlobal :: (Alternative m, MonadIO m, Show (Rec ctxt)) => Int -> World ctxt -> K (Bool, World ctxt) Tabula m Bool
semGlobal propIdx here = K \k ctx -> case view (ixWorld here) ctx of
  Just node -> empty
  Nothing -> do
    let node =
          view (ixWorld here) ctx
            & fromMaybe mempty
            & set (ixProp propIdx) True

        ctx' =
          ctx
            & ixWorld here .~ Just node
            & ixDepth %~ (1 +)

    liftIO $ putStrLn (show (view ixDepth ctx) ++ ": " ++ show here)

    k True ctx' >>= \case
      (False, here') -> return (False, here')
      (True, here') -> return (True, here') <|> k True ctx'

-- layer :: (MonadIO m, Show a) => Cofree (LogicT m) a -> Queue (LogicT m) (LevelsT m a)
-- layer (here :< there) = liftA2 (<|>) (now (pure (pure here))) (later (wrap (fmap layer there)))

-- toSearch ::
--   Show (Rec ctxt) =>
--   String ->
--   Cofree (LogicT (Except [MCError ctxt])) (Set (Interval (World ctxt) (Set (World ctxt)))) ->
--   LevelsT (Except [MCError ctxt]) (Int, Set (Interval (World ctxt) (Set (World ctxt))))
-- toSearch prop w = fromLogicT (runQueue (getG (semGlobal prop w))) >>= fromSearchSpace 0

-- fromSearchSpace ::
--   Monad m =>
--   Int ->
--   Cofree (LogicT m) (Set (Interval a (Set a))) ->
--   LevelsT m (Int, Set (Interval a (Set a)))
-- fromSearchSpace depth (here :< there)
--   | Set.null here = empty -- pure (0, Set.empty)
--   | otherwise = pure (depth, here) <|> (fromLogicT there >>= fromSearchSpace (1 + depth))

fromLogicT :: Monad f => LogicT f a -> LevelsT f a
fromLogicT (LogicT m) = wrapLevelsT (m (fmap . (<|>) . pure) (pure empty))

-- result <-
--   modelCheck reactive initialWorlds
--     & runSearchT
--     & runLevelsM
--     & runBaseT
--     & flip runReaderT (MCEnv actInfo)
--     & flip evalStateT (MCState Set.empty)
--     & runExcept
--     & pure

-- return result

-- modelCheck ::
--   (Show (Rec ctxt), Ord (Rec ctxt)) =>
--   RSet (World ctxt) (Set (Interval (World ctxt) (Set (World ctxt)))) ->
--   Set (World ctxt) ->
--   SearchT ctxt Identity ()
-- modelCheck reactive initials = do
--   globalQuals <- Map.keys . Map.filter (propInfoIsAlways . snd) <$> view mcEnvPropInfo
--   futureQuals <- Map.keys . Map.filter (propInfoIsEventually . snd) <$> view mcEnvPropInfo
--   staysQuals <- Map.keys . Map.filter (propInfoIsStaysAs . snd) <$> view mcEnvPropInfo

--   Debug.trace (show futureQuals) (pure ())
--   Debug.trace (show globalQuals) (pure ())
--   Debug.trace (show staysQuals) (pure ())

--   let stream = pruneCycles (streamBehaviors globalQuals reactive initials)

--   xs <- searchFuture staysQuals stream
--   Debug.trace (show xs) (pure ())

--   return ()

-- ---------------------------------------------------------------------------------------------------------------------

-- toSearchT ::
--   (Show a) =>
--   Int ->
--   Cofree (Except [MCError ctxt]) (Set (Interval a (String, a))) ->
--   SearchT ctxt Identity (Int, Set (Interval a (String, a)))
-- toSearchT depth (here :< there)
--   | Set.null here = case runExcept there of
--     Left errs -> throwError errs
--     Right _ -> pure (depth, Set.empty)
--   | otherwise = case runExcept there of
--     Left errs -> throwError errs
--     Right nexts -> pure (depth, here) <|> toSearchT (1 + depth) nexts

-- searchFuture ::
--   Show (Rec ctxt) =>
--   [String] ->
--   Cofree (Except [MCError ctxt]) (Set (Interval (World ctxt) (Set (World ctxt)))) ->
--   SearchT ctxt Identity (Set (Interval (World ctxt) (Set (World ctxt))))
-- searchFuture names w =
--   let futures = foldr (tensor . fmap (fmap snd . toSearchT 0) . getF) (pure (pure Set.empty)) (makeFutures names w)
--    in case runExcept (runQueue futures) of
--         Left errs -> throwError errs
--         Right intervals -> intervals
--   where
--     makeFutures ::
--       Show (Rec ctxt) =>
--       [String] ->
--       Cofree (Except [MCError ctxt]) (Set (Interval (World ctxt) (Set (World ctxt)))) ->
--       [F (Except [MCError ctxt]) (Set (Interval (World ctxt) (Set (World ctxt))))]
--     makeFutures names w = foldMap (pure . (`semStaysF` w)) names

-- semFuture ::
--   (Show (Rec ctxt)) =>
--   String ->
--   Cofree (Except [MCError ctxt]) (Set (Interval (World ctxt) (String, World ctxt))) ->
--   F (Except [MCError ctxt]) (Set (Interval (World ctxt) (String, World ctxt)))
-- semFuture name (here :< there)
--   | Set.null here = error ("eventually error: " ++ name)
--   | otherwise =
--     let witnesses = Set.filter ((== name) . fst . timeAfter) here
--      in if Set.null witnesses
--           then case runExcept there of
--             Left errs -> F (now (throwError errs))
--             Right nexts -> Debug.trace ("F(" ++ show name ++ "): " ++ show here) (delayF (semFuture name nexts))
--           else captureF (here :< there)

-- semStaysF ::
--   (Show (Rec ctxt)) =>
--   String ->
--   Cofree (Except [MCError ctxt]) (Set (Interval (World ctxt) (String, World ctxt))) ->
--   F (Except [MCError ctxt]) (Set (Interval (World ctxt) (String, World ctxt)))
-- semStaysF name (here :< there)
--   | Set.null here = undefined
--   | otherwise =
--     let witnesses = Set.filter ((== name) . fst . timeAfter) here
--      in case runExcept there of
--           Left errs -> F (now (throwError errs))
--           Right nexts
--             | Set.null witnesses -> Debug.trace ("FG[F x](" ++ show name ++ "): " ++ show here) (delayF (semStaysF name nexts))
--             | otherwise -> Debug.trace ("FG[F !](" ++ show name ++ "): " ++ show here) (delayF (semStaysG name nexts))

-- semStaysG ::
--   (Show (Rec ctxt)) =>
--   String ->
--   Cofree (Except [MCError ctxt]) (Set (Interval (World ctxt) (String, World ctxt))) ->
--   F (Except [MCError ctxt]) (Set (Interval (World ctxt) (String, World ctxt)))
-- semStaysG name (here :< there)
--   | Set.null here = undefined
--   | otherwise =
--     let witnesses = Set.filter ((== name) . fst . timeAfter) here
--      in case runExcept there of
--           Left errs -> F (now (throwError errs))
--           Right nexts
--             | Set.null witnesses -> Debug.trace ("FG[G x](" ++ name ++ "): " ++ show here) (delayF (semStaysF name nexts))
--             | otherwise -> Debug.trace ("FG[G !](" ++ name ++ "): " ++ show here) (delayF (semStaysG name nexts))

type TestSpec =
  Spec
    (Var "x" Int :. Var "y" Int) -- :. Var "z" Int)
    ("incX" !> 'Unfair \/ "incY" !> 'Unfair) -- \/ "incZ" !> 'Unfair)
    (Always "incX" /\ Eventually "incZ") -- /\ Eventually (Always "incY"))

incX :: Action (VariableCtxt TestSpec) Bool
incX = do
  x <- plain #x
  if x < 4
    then do
      n <- oneOf [1 .. 2]
      #x .= pure (n + x)
    else do
      -- nil <- oneOf []
      #x .= pure 0
  return True

incY :: Action (VariableCtxt TestSpec) Bool
incY = do
  x <- plain #x
  y <- plain #y
  #y .= pure 1 -- (x + y `mod` 5)
  return True

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
