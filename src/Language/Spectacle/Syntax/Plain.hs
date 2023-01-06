
-- |
-- Module      :  Language.Spectacle.Syntax.Plain
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Plain or known variable usage and substitution.
--
-- @since 1.0.0
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
import Data.Type.Rec (Has, Name, Rec)
import qualified Data.Type.Rec as Rec
import Language.Spectacle.Lang (Lang (Op, Pure, Scoped), Member, decomposeOp, decomposeS, scope)
import Language.Spectacle.Syntax.Plain.Internal (Effect (PlainVar), Plain (Plain))

-- -------------------------------------------------------------------------------------------------

-- | 'plain' for a variable named @s@ is the value of @s@ from the previous frame of time.
--
-- @since 1.0.0
plain :: (Member Plain effs, Has s a ctx) => Name s -> Lang ctx effs a
plain nm = scope (PlainVar nm)
{-# INLINE plain #-}

-- | Discharge a 'Plain' effect, substituting instances of 'PlainVar' for the values in the given
-- 'Data.Type.Rec'.
--
-- @since 1.0.0
runPlain :: Rec ctx -> Lang ctx (Plain ': effs) a -> Lang ctx effs a
runPlain vars = \case
  Pure x -> pure x
  Op op k -> case decomposeOp op of
    Left other -> Op other (runPlain vars . k)
    Right bottom -> absurd (coerce bottom)
  Scoped scoped loom -> case decomposeS scoped of
    Left other -> Scoped other loom'
    Right (PlainVar name) -> do
      runLoom loom' (pure $ Rec.get name vars)
    where
      loom' = loom ~>~ hoist (runPlain vars)
