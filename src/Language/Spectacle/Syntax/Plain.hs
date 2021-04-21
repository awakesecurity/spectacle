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
  ( Effect,
    Lang (Pure, Yield),
    Member,
    Union (Op, Scoped),
    decomposeOp,
    decomposeS,
    scope,
  )
import Language.Spectacle.Syntax.Plain.Internal (Effect (PlainVar), Plain (Plain))

-- -------------------------------------------------------------------------------------------------

-- | 'plain' for a variable named @s@ is the value of @s@ from the previous frame of time.
--
-- @since 0.1.0.0
plain :: (s # a .| ctx, Member Plain effs) => Name s -> Lang ctx effs a
plain name = scope (PlainVar name)
{-# INLINE plain #-}

-- | Discharge a 'Plain' effect, substituting instances of 'PlainVar' for the values in the given
-- 'Data.Type.Rec'.
--
-- @since 0.1.0.0
runPlain :: Rec ctx -> Lang ctx (Plain ': effs) a -> Lang ctx effs a
runPlain vars = \case
  Pure x -> pure x
  Yield (Op op) k -> case decomposeOp op of
    Left other -> Yield (Op other) (runPlain vars . k)
    Right bottom -> absurd (coerce bottom)
  Yield (Scoped scoped loom) k -> case decomposeS scoped of
    Left other -> Yield (Scoped other loom') k'
    Right (PlainVar name) -> do
      let x = getRec name vars
      runLoom loom' (pure x) >>= k'
    where
      loom' = loom ~>~ hoist (runPlain vars)

      k' = runPlain vars . k
