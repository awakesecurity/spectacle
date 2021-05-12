{-# LANGUAGE MultiWayIf #-}

module Spectacle.Syntax.Modal.Properties
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
import Language.Spectacle.Syntax.Modal.Preterm
import Language.Spectacle.Syntax.Modal.Internal
import Language.Spectacle.Syntax.NonDet
import Language.Spectacle.Syntax.Plain
import Language.Spectacle.Syntax.Prime

-- ---------------------------------------------------------------------------------------------------------------------

pretermsOf ::
  Lang '[] '[Modal, Logic, Prime, Plain, Error RuntimeException] Bool ->
  Preterm Bool ->
  PropertyT IO ()
pretermsOf effs matches = do
  let preterms =
        effs
          & materialize
          & fmap rewritePreterm
          & getPreterms RNil RNil
  case preterms of
    Right expr ->
      if matches == expr
        then success
        else do
          annotate "Rewritten expression failed to match"
          annotate $ "expected: " ++ show matches
          annotate $ "saw: " ++ show expr
          failure
    Left exc -> do
      annotate "Rewriting failed with exception:"
      annotateShow exc
      failure

-- | Always is idemponent.
--
-- @
-- always (always p) = always p
-- @
prop_alwaysIdempotent :: Property
prop_alwaysIdempotent =
  withTests 1 . property $
    pretermsOf alwaysAlways (PreAlways (PreConst True))
  where
    alwaysAlways :: Member Modal effs => Lang ctx effs Bool
    alwaysAlways = always (always (pure True))

-- | Eventually and always absorb always
--
-- @
-- always (eventually (always p)) = eventually (always p)
-- @
prop_alwaysAbsorption :: Property
prop_alwaysAbsorption =
  withTests 1 . property $
    pretermsOf alwaysEventuallyAlways (PreUpUntil (PreConst True) (PreAlways (PreConst True)))
  where
    alwaysEventuallyAlways :: Member Modal effs => Lang ctx effs Bool
    alwaysEventuallyAlways = always (eventually (always (pure True)))

-- | Eventually is idempotent.
--
-- @
-- eventually (eventually p) = eventually p
-- @
prop_eventuallyIdempotent :: Property
prop_eventuallyIdempotent =
  withTests 1 . property $
    pretermsOf eventuallyEventually (PreUpUntil (PreConst True) (PreConst True))
  where
    eventuallyEventually :: Member Modal effs => Lang ctx effs Bool
    eventuallyEventually = eventually (eventually (pure True))

-- | Always and eventually absorb eventually.
--
-- @
-- eventually (always (eventually p)) = eventually (always p)
-- @
prop_eventuallyAbsorption :: Property
prop_eventuallyAbsorption =
  withTests 1 . property $
    pretermsOf eventuallyAlwaysEventually (PreAlways (PreUpUntil (PreConst True) (PreConst True)))
  where
    eventuallyAlwaysEventually :: Member Modal effs => Lang ctx effs Bool
    eventuallyAlwaysEventually = eventually (always (eventually (pure True)))

-- | Eventually is the dual of always.
--
-- @
-- complement (always p) = eventually (complement p)
-- @
prop_alwaysDual :: Property
prop_alwaysDual =
  withTests 1 . property $
    pretermsOf complementAlways (PreUpUntil (PreConst True) (PreComplement (PreConst True)))
  where
    complementAlways :: Members '[Logic, Modal] effs => Lang ctx effs Bool
    complementAlways = complement (always (pure True))

-- | Always is the dual of eventually.
--
-- @
-- complement (eventually p) = always (complement p)
-- @
prop_eventuallyDual :: Property
prop_eventuallyDual =
  withTests 1 . property $
    pretermsOf complementEventually (PreAlways (PreComplement (PreConst True)))
  where
    complementEventually :: Members '[Logic, Modal] effs => Lang ctx effs Bool
    complementEventually = complement (eventually (pure True))

-- | Negation distributes over conjunction.
--
-- @
-- complement (conjunct p q) = disjunct (complement p) (complement q)
-- @
prop_negDistributesAnd :: Property
prop_negDistributesAnd =
  withTests 1 . property $
    pretermsOf complementConjunct (PreDisjunct (PreComplement (PreConst True)) (PreComplement (PreConst True)))
  where
    complementConjunct :: Member Logic effs => Lang ctx effs Bool
    complementConjunct = complement (pure True `conjunct` pure True)

-- | Negation distributes over disjunction.
--
-- @
-- complement (disjunct p q) = conjunct (complement p) (complement q)
-- @
prop_negDistributesOr :: Property
prop_negDistributesOr =
  withTests 1 . property $
    pretermsOf complementDisjunct (PreConjunct (PreComplement (PreConst True)) (PreComplement (PreConst True)))
  where
    complementDisjunct :: Member Logic effs => Lang ctx effs Bool
    complementDisjunct = complement (pure True `disjunct` pure True)

-- | Negation is involutional.
--
-- @
-- complement (complement p) = p
-- @
prop_negInvolute :: Property
prop_negInvolute =
  withTests 1 . property $
    pretermsOf complementComplement (PreConst True)
  where
    complementComplement :: Member Logic effs => Lang ctx effs Bool
    complementComplement = complement (complement (pure True))

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
    , testProperty "Negation distributes over conjunction" prop_negDistributesAnd
    , testProperty "Negation distributes over disjunction" prop_negDistributesOr
    , testProperty "Negation is involutional" prop_negInvolute
    ]
