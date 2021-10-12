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
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Functor.Identity
import Data.Hashable
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
import Data.Bifunctor
import Data.Context
import Data.Foldable
import Data.Functor.Contravariant
import Data.Kind
import Data.Semigroup
import Data.Temporal
import Data.Tree
import Data.Type.Rec hiding (concat)
import Data.World
import Language.Spectacle.AST
import Language.Spectacle.Behavior
import Language.Spectacle.Checker.Fingerprint
import Language.Spectacle.Checker.MCError
import Language.Spectacle.Lang
import Language.Spectacle.Model.Internal
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

setupRSet ::
  Hashable (Rec ctxt) =>
  [(Fairness, String, Action ctxt Bool)] ->
  RSet (World ctxt) (Set (String, Set (World ctxt)))
setupRSet xs = RSet \world -> foldMap (`getRSet` world) (makeRSet xs)
  where
    makeRSet = map \(_, ident, act) -> proc dom -> do
      codom <- fromAction act -< dom
      returnA -< Set.singleton (ident, codom)

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
      reactive = setupRSet actions

  case initialTimes vs of
    Left err -> return (Left err)
    Right initialWorlds -> do
      result <-
        modelCheck reactive initialWorlds
          & runSearchT
          & runLevelsM
          & runBaseT
          & flip runReaderT (MCEnv actInfo)
          & flip evalStateT (MCState Set.empty)
          & runExcept
          & pure

      return result

modelCheck ::
  (Show (Rec ctxt), Ord (Rec ctxt)) =>
  RSet (World ctxt) (Set (String, Set (World ctxt))) ->
  Set (World ctxt) ->
  SearchT ctxt Identity ()
modelCheck reactive initials = do
  globalQuals <- Map.keys . Map.filter (propInfoIsAlways . snd) <$> view mcEnvPropInfo
  futureQuals <- Map.keys . Map.filter (propInfoIsEventually . snd) <$> view mcEnvPropInfo

  Debug.trace (show futureQuals) (pure ())

  let stream = pruneCycles (streamBehaviors globalQuals reactive initials)

  searchFuture futureQuals stream

  return ()

-- ---------------------------------------------------------------------------------------------------------------------

toSearchT ::
  (Show a) =>
  Int ->
  Cofree (Except [MCError ctxt]) (Set (Interval a (String, a))) ->
  SearchT ctxt Identity (Int, Set (Interval a (String, a)))
toSearchT depth (here :< there)
  | Set.null here = case runExcept there of
    Left errs -> throwError errs
    Right _ -> pure (depth, Set.empty)
  | otherwise = case runExcept there of
    Left errs -> throwError errs
    Right nexts -> pure (depth, here) <|> toSearchT (1 + depth) nexts

searchFuture ::
  Show (Rec ctxt) =>
  [String] ->
  Cofree (Except [MCError ctxt]) (Set (Interval (World ctxt) (String, World ctxt))) ->
  SearchT ctxt Identity (Set (Interval (World ctxt) (String, World ctxt)))
searchFuture names w =
  let futures = foldr (tensor . fmap (fmap snd . toSearchT 0) . getF) (pure (pure Set.empty)) (makeFutures names w)
   in case runExcept (runQueue futures) of
        Left errs -> throwError errs
        Right intervals -> intervals
  where
    makeFutures ::
      Show (Rec ctxt) =>
      [String] ->
      Cofree (Except [MCError ctxt]) (Set (Interval (World ctxt) (String, World ctxt))) ->
      [F (Except [MCError ctxt]) (Set (Interval (World ctxt) (String, World ctxt)))]
    makeFutures names w = foldMap (pure . (`semFuture` w)) names

semFuture ::
  (Show (Rec ctxt)) =>
  String ->
  Cofree (Except [MCError ctxt]) (Set (Interval (World ctxt) (String, World ctxt))) ->
  F (Except [MCError ctxt]) (Set (Interval (World ctxt) (String, World ctxt)))
semFuture name (here :< there)
  | Set.null here = error ("eventually error: " ++ name)
  | otherwise =
    let witnesses = Set.filter ((== name) . fst . timeAfter) here
     in if Set.null witnesses
          then case runExcept there of
            Left errs -> F (now (throwError errs))
            Right nexts -> delayF (semFuture name nexts)
          else captureF (here :< there)

semStays ::
  (Show (Rec ctxt)) =>
  String ->
  Cofree (Except [MCError ctxt]) (Set (Interval (World ctxt) (String, World ctxt))) ->
  F (Except [MCError ctxt]) (Set (Interval (World ctxt) (String, World ctxt)))
semStays name (here :< there)
  | Set.null here = undefined
  | otherwise = _
    let witnesses = Set.filter ((== name) . fst . timeAfter) here
     in if Set.null witnesses
          then case runExcept there of
                 Left errs -> F (now (throwError errs))
                 Right nexts -> _
          else _

type TestSpec =
  Spec
    (Var "x" Int :. Var "y" Int :. Var "z" Int)
    ("incX" !> 'Unfair \/ "incY" !> 'Unfair \/ "incZ" !> 'Unfair)
    (Always "incX" /\ Eventually "incZ" /\ Eventually "incY")

incX :: Action (VariableCtxt TestSpec) Bool
incX = do
  x <- plain #x
  if x < 10
    then do
      #x .= pure (1 + x)
    else do
      #x .= pure 11
  return True

incY :: Action (VariableCtxt TestSpec) Bool
incY = do
  x <- plain #x
  y <- plain #y
  if x == 10
    then
      if y < 2
        then do
          #x .= pure 0
          #y .= pure (1 + y)
        else do
          #y .= pure 0
    else do
      nil <- oneOf []
      #y .= pure nil
  return True

incZ :: Action (VariableCtxt TestSpec) Bool
incZ = do
  x <- plain #x
  z <- plain #z
  #z .= pure 1
  return (x == 5)

spec :: TestSpec
spec =
  Spec
    (#x := pure 0 :. #y := pure 0 :. #z := pure 0)
    (UnfairAction #incX incX :\/: UnfairAction #incY incY :\/: UnfairAction #incZ incZ)

testSpec :: IO ()
testSpec =
  runModelCheck spec >>= \case
    Left errs -> mapM_ print errs
    Right xs -> print xs -- xs
