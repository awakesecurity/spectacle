module Language.Spectacle.AST.Invariant
  ( type Invariant,
    type InvariantSyntax,
    runInvariant,
    applyRewrites,
    getL4Terms,
  )
where

import Data.Function ((&))

import Data.Type.Rec (Rec)
import Language.Spectacle.AST.Invariant.Internal
  ( Invariant,
    InvariantSyntax,
  )
import Language.Spectacle.Exception.RuntimeException (RuntimeException (SyntaxException))
import Language.Spectacle.Lang (Lang, Member, runLang)
import Language.Spectacle.Syntax.Error (Error, runError, throwE)
import Language.Spectacle.Syntax.Fresh (Fresh, runFresh)
import Language.Spectacle.Syntax.Logic (Logic)
import Language.Spectacle.Syntax.Modal
  ( LTerm,
    Level (L4),
    Modal,
    Preterm,
    SyntaxLevel (fromPreterm),
    abstract,
    materialize,
    normalizePreterm,
  )
import Language.Spectacle.Syntax.Plain (runPlain)

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'runInvariant' sends an 'Invariant' to its equivalent, reduced 'LTerm' representation.
--
-- @since 0.1.0.0
runInvariant :: Rec ctx -> Invariant ctx Bool -> Either RuntimeException (LTerm 'L4 Bool)
runInvariant st formula =
  formula
    & materialize
    & (>>= getL4Terms)
    & runPlain st
    & runFresh 0
    & runError
    & runLang
    & fmap snd
{-# INLINE runInvariant #-}

-- | Normalizes a temporal formula.
--
-- @since 0.1.0.0
applyRewrites :: Member Fresh effs => Lang ctx (Modal ': Logic ': effs) Bool -> Lang ctx (Modal ': Logic ': effs) Bool
applyRewrites formula =
  formula
    & materialize
    & (>>= normalizePreterm)
    & abstract
{-# INLINE applyRewrites #-}

-- | Sends 'Preterm' to 'LTerm' in a 'Lang'.
--
-- @since 0.1.0.0
getL4Terms :: Member (Error RuntimeException) effs => Preterm Bool -> Lang ctx effs (LTerm 'L4 Bool)
getL4Terms preterms = case fromPreterm @ 'L4 preterms of
  Left exc -> throwE (SyntaxException exc)
  Right lterms -> return lterms
{-# INLINE getL4Terms #-}
