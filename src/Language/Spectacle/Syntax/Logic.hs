-- | Quantifiers and logic.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Logic
  ( Logic (Logic),
    Effect (Complement, Conjunct, Disjunct),
    complement,
    conjunct,
    disjunct,
    implies,
    iff,
    runLogic,
  )
where

import Control.Applicative (Alternative ((<|>)), Applicative (liftA2))
import Data.Coerce (coerce)
import Data.Void (absurd)

import Data.Functor.Loom (hoist, runLoom, (~>~))
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Lang
  ( Lang (Op, Pure, Scoped),
    Members,
    Member,
    decomposeOp,
    decomposeS,
    scope,
  )
import Language.Spectacle.Syntax.Error (Error)
import Language.Spectacle.Syntax.Logic.Internal (Effect (..), Logic (Logic))
import Language.Spectacle.Syntax.NonDet (NonDet)

-- ---------------------------------------------------------------------------------------------------------------------

-- | Logical negation. The 'complement' operator is equivalent to 'not' for simple expressions, but
-- can be used to negate quantifiers and the other logical operators in spectacle.
--
-- @since 0.1.0.0
complement :: Member Logic effs => Lang ctx effs Bool -> Lang ctx effs Bool
complement m = scope (Complement m)
{-# INLINE complement #-}

-- | Boolean conjunction.
--
-- @since 0.1.0.0
conjunct :: Member Logic effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
conjunct m n = scope (Conjunct m n)
{-# INLINE conjunct #-}

-- | Boolean disjunction.
--
-- @since 0.1.0.0
disjunct :: Member Logic effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
disjunct m n = scope (Disjunct m n)
{-# INLINE disjunct #-}

-- | Logical implication.
--
-- @since 0.1.0.0
implies :: Member Logic effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
implies m n = complement (conjunct m (complement n))
{-# INLINE implies #-}

-- | If and only if.
--
-- @since 0.1.0.0
iff :: Member Logic effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
iff m n = conjunct (implies m n) (implies n m)
{-# INLINE iff #-}

-- | Discharge a 'Logic' effect.
--
-- @since 0.1.0.0
runLogic ::
  forall ctx effs.
  Members '[Error RuntimeException, NonDet] effs =>
  Lang ctx (Logic ': effs) Bool ->
  Lang ctx effs Bool
runLogic = \case
  Pure x -> pure x
  Op op k -> case decomposeOp op of
    Left other -> Op other (runLogic . k)
    Right bottom -> absurd (coerce bottom)
  Scoped scoped loom -> case decomposeS scoped of
    Left other -> Scoped other loom'
    Right (Complement m) -> runLoom loom' (fmap not m)
    Right (Conjunct m n) -> liftA2 (&&) (runLoom loom' m) (runLoom loom' n)
    Right (Disjunct m n) -> runLoom loom' m <|> runLoom loom' n
    where
      loom' = loom ~>~ hoist runLogic
