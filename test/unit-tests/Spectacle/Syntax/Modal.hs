{-# LANGUAGE MultiWayIf #-}

module Spectacle.Syntax.Modal
  ( tests,
  )
where

import Control.Arrow
import Control.Monad
import Data.Function ((&))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Data.Type.Rec
import Language.Spectacle.Exception.RuntimeException
import Language.Spectacle.Lang
import Language.Spectacle.Syntax.Error
import Language.Spectacle.Syntax.Logic
import Language.Spectacle.Syntax.Modal
import Language.Spectacle.Syntax.Modal.Internal
import Language.Spectacle.Syntax.NonDet
import Language.Spectacle.Syntax.Plain
import Language.Spectacle.Syntax.Prime

-- -------------------------------------------------------------------------------------------------

pretermsOf ::
  Lang '[] '[Modal, Logic, Prime, Plain, NonDet, Error RuntimeException] Bool ->
  Preterm Bool ->
  PropertyT IO ()
pretermsOf effs matches = do
  let preterms =
        effs
          & materialize
          & fmap rewritePreterm
          & getPreterms RNil RNil
  case preterms of
    Right [expr] ->
      if matches == expr
        then success
        else do
          annotate "Rewritten expression failed to match"
          annotate $ "expected: " ++ show matches
          annotate $ "saw: " ++ show expr
          failure
    Right xs -> do
      annotate "Rewriting expression failed with multiple values:"
      annotateShow xs
      failure
    Left exc -> do
      annotate "Rewriting failed with exception:"
      annotateShow exc
      failure

prop_alwaysIdempotent :: Property
prop_alwaysIdempotent =
  withTests 1 . property $
    pretermsOf alwaysAlways (PreAlways (PreConst True))
  where
    alwaysAlways :: Member Modal effs => Lang ctx effs Bool
    alwaysAlways = always (always (pure True))

prop_alwaysAbsorption :: Property
prop_alwaysAbsorption =
  withTests 1 . property $
    pretermsOf alwaysEventuallyAlways (PreUpUntil (PreConst True) (PreAlways (PreConst True)))
  where
    alwaysEventuallyAlways :: Member Modal effs => Lang ctx effs Bool
    alwaysEventuallyAlways = always (eventually (always (pure True)))

prop_eventuallyIdempotent :: Property
prop_eventuallyIdempotent =
  withTests 1 . property $
    pretermsOf eventuallyEventually (PreUpUntil (PreConst True) (PreConst True))
  where
    eventuallyEventually :: Member Modal effs => Lang ctx effs Bool
    eventuallyEventually = eventually (eventually (pure True))

prop_eventuallyAbsorption :: Property
prop_eventuallyAbsorption =
  withTests 1 . property $
    pretermsOf eventuallyAlwaysEventually (PreAlways (PreUpUntil (PreConst True) (PreConst True)))
  where
    eventuallyAlwaysEventually :: Member Modal effs => Lang ctx effs Bool
    eventuallyAlwaysEventually = eventually (always (eventually (pure True)))

prop_alwaysDual :: Property
prop_alwaysDual =
  withTests 1 . property $
    pretermsOf complementAlways (PreUpUntil (PreConst True) (PreComplement (PreConst True)))
  where
    complementAlways :: Members '[Logic, Modal] effs => Lang ctx effs Bool
    complementAlways = complement (always (pure True))

prop_eventuallyDual :: Property
prop_eventuallyDual =
  withTests 1 . property $
    pretermsOf complementEventually (PreAlways (PreComplement (PreConst True)))
  where
    complementEventually :: Members '[Logic, Modal] effs => Lang ctx effs Bool
    complementEventually = complement (eventually (pure True))

tests :: TestTree
tests =
  testGroup
    "Spectacle.Syntax.Modal"
    [ testProperty "Always idempotency" prop_alwaysIdempotent
    , testProperty "Always absorption" prop_alwaysAbsorption
    , testProperty "Eventually idempotency" prop_eventuallyIdempotent
    , testProperty "Eventually absorption" prop_eventuallyAbsorption
    , testProperty "Always dual" prop_alwaysDual
    , testProperty "Eventually dual" prop_eventuallyDual
    ]
