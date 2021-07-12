{-# LANGUAGE OverloadedLabels #-}

module Spectacle.Checker.Model where

import Data.Set (Set)
import qualified Data.Set as Set
import Hedgehog
  ( Property,
    PropertyT,
    annotate,
    annotateShow,
    failure,
    property,
    success,
    withTests,
  )
import Numeric.Natural
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Data.Sequence as Seq

import Data.Type.Rec
import Language.Spectacle.Checker.CoverageMap (prettyShow)
import Language.Spectacle.Checker.Fingerprint
import Language.Spectacle.Checker.Model
import Language.Spectacle.Checker.Universe
import Language.Spectacle.Checker.World
import Language.Spectacle.Checker.Fairness
import Language.Spectacle.Checker.Model.ModelContext
import Language.Spectacle.Exception
import Language.Spectacle.Syntax (always, oneOf)
import Spectacle.Checker.Actions

-- ---------------------------------------------------------------------------------------------------------------------

quotient_inc_prop :: IO ()
quotient_inc_prop = do
  let modelResult :: (Either SpecException [(World IncSpec, [Fingerprint])], Universe)
      modelResult = runModel context mempty model
  print $ modelResult
  where
    initWorld :: World '["x" # Natural]
    initWorld = newWorld (RCon #x 1 RNil)

    context :: ModelContext IncSpec
    context = ModelContext unfair (oneOf [1] >>= actionInc) (always (pure True)) Seq.empty Set.empty []

    model :: Model IncSpec (World IncSpec)
    model =
      stepModel initWorld
        >>= stepModel
        >>= stepModel
        >>= stepModel

quotient_or_prop :: IO ()
quotient_or_prop = do
  let modelResult :: (Either SpecException [(World OrSpec, [Fingerprint])], Universe)
      modelResult = runModel context mempty model
  putStrLn . show $ modelResult
  print (fst modelResult)
  where
    initWorld :: World '["x" # Natural]
    initWorld = newWorld (RCon #x 1 RNil)

    context :: ModelContext OrSpec
    context = ModelContext unfair actionOr (always (pure True)) Seq.empty Set.empty []

    model :: Model OrSpec (World OrSpec)
    model =
      stepModel initWorld
        >>= stepModel
        >>= stepModel
        >>= stepModel
