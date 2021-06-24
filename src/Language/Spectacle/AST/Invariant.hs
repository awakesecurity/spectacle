module Language.Spectacle.AST.Invariant
  ( type Invariant,
    type InvariantSyntax,
    runInvariant,
    applyRewrites,
    getL4Terms,
  )
where

import Data.Function ((&))
import Data.Functor ((<&>))

import Data.Type.Rec (Rec)
import Language.Spectacle.AST.Invariant.Internal
  ( Invariant,
    InvariantSyntax,
  )
import Language.Spectacle.Exception.RuntimeException (RuntimeException (SyntaxException))
import Language.Spectacle.Lang (Lang, Member, runLang)
import Language.Spectacle.Syntax.Enabled (runEnabled)
import Language.Spectacle.Syntax.Error (Error, runError, throwE)
import Language.Spectacle.Syntax.Logic (Logic)
import Language.Spectacle.Syntax.Modal
  ( Modal,
    Preterm,
    normalForm,
    pretermFromModal,
    pretermToModal,
  )
import Language.Spectacle.Syntax.Modal.Term
  ( Term,
    runNameSupply,
    termFromPreterm,
  )
import Language.Spectacle.Syntax.Plain (runPlain)
import Language.Spectacle.Syntax.Prime (substPrime)

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'runInvariant' sends an 'Invariant' to its equivalent, reduced 'LTerm' representation.
--
-- @since 0.1.0.0
runInvariant :: Bool -> Rec ctx -> Rec ctx -> Invariant ctx Bool -> Either RuntimeException (Term Bool)
runInvariant isEnabled worldHere worldThere formula =
  formula
    & pretermFromModal
    & (>>= getL4Terms)
    & runEnabled isEnabled
    & substPrime worldThere
    & runPlain worldHere
    & runError
    & runLang
{-# INLINE runInvariant #-}

-- | Normalizes a temporal formula.
--
-- @since 0.1.0.0
applyRewrites :: Lang ctx (Modal ': Logic ': effs) Bool -> Lang ctx (Modal ': Logic ': effs) Bool
applyRewrites formula =
  formula
    & pretermFromModal
    <&> normalForm
    & pretermToModal
{-# INLINE applyRewrites #-}

-- | Sends 'Preterm' to 'Term' in a 'Lang'.
--
-- @since 0.1.0.0
getL4Terms :: Member (Error RuntimeException) effs => Preterm Bool -> Lang ctx effs (Term Bool)
getL4Terms preterms = case runNameSupply (termFromPreterm preterms) of
  Left exc -> throwE (SyntaxException exc)
  Right terms -> return terms
{-# INLINE getL4Terms #-}
