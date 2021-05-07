-- | Plain or known variable usage and substitution.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Plain
  ( Plain (Plain),
    Effect (PlainVar),
    plain,
    runPlain,
  )
where

import Data.Coerce (coerce)
import Data.Void (absurd)

import Data.Functor.Loom (hoist, runLoom, (~>~))
import Data.Type.Rec (Name, Rec, getRec, type (#), type (.|))
import Language.Spectacle.Lang
  ( Lang (Pure, Op, Scoped),
    Members,
    decomposeOp,
    decomposeS,
    scope,
  )
import Language.Spectacle.Syntax.Plain.Internal (Effect (PlainVar), Plain (Plain))

-- -------------------------------------------------------------------------------------------------

-- | 'plain' for a variable named @s@ is the value of @s@ from the previous frame of time.
--
-- @since 0.1.0.0
plain :: (s # a .| ctx, Members Plain effs) => Name s -> Lang ctx effs a
plain name = scope (PlainVar name)
{-# INLINE plain #-}

-- | Discharge a 'Plain' effect, substituting instances of 'PlainVar' for the values in the given
-- 'Data.Type.Rec'.
--
-- @since 0.1.0.0
runPlain :: Rec ctx -> Lang ctx (Plain ': effs) a -> Lang ctx effs a
runPlain vars = \case
  Pure x -> pure x
  Op op k -> case decomposeOp op of
    Left other -> Op other (runPlain vars . k)
    Right bottom -> absurd (coerce bottom)
  Scoped scoped loom -> case decomposeS scoped of
    Left other -> Scoped other loom'
    Right (PlainVar name) -> do
      let x = getRec name vars
      runLoom loom' (pure x)
    where
      loom' = loom ~>~ hoist (runPlain vars)
