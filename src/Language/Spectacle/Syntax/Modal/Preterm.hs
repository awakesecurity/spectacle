{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module houses 'Preterm's and associated functions. 'Preterm's serve as an intermediate representation for
-- rewriting formula as equivalent expressions in disjunctive normal. The rewrite rules used to normalize temporal
-- formula are chosen so temporal operators are factored out wherever possible to optimize checking.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Modal.Preterm
  ( -- * Preterms Preterm
    Preterm
      ( PreConst,
        PreConjunct,
        PreDisjunct,
        PreComplement,
        PreAlways,
        PreEventually,
        PreUpUntil
      ),

    -- * Rewriting
    Rewrite (Rewrite, runRewrite),
    normalForm,
    distributeRewrites,
    applyRewrite,

    -- ** Rewrite Rules
    redexAlternatives,
    redexNegInvolute,
    redexAlwaysDual,
    redexAlwaysFactorsAnd,
    redexEventuallyDual,
    redexNegDistribAnd,
    redexNegDistribOr,
    redexEventuallyIdempotent,
    redexEventuallyDistribOr,
    redexEventuallyAbsorbs,
    redexAlwaysIdempotent,
    redexAlwaysAbsorbs,
    redexAndDistribOrLeft,
    redexAndDistribOrRight,
  )
where

import Control.Applicative (Alternative ((<|>)))
import GHC.Stack (SrcLoc)

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'Preterm' is the raw underlying syntax tree representing a temporal formula modulo the continuation or
-- 'Data.Functor.Loom' information. A tree of 'Preterm's can be built by temporarily discharges the 'Modal' and 'Logic'
-- effects with 'pretermFromModal' and converted back into their effectful representation with 'pretermToModal', which
-- forms an isomorphism:
--
-- @
--                                                   'pretermFromModal'
--                                   ───────────────────────────────────────────────>
-- Members '[Modal, Logic] effs =>                                                      Preterm Bool
--   Lang ctx effs Bool              <───────────────────────────────────────────────
--                                                    'pretermToModal'
-- @
--
-- @since 0.1.0.0
data Preterm a where
  PreConst :: a -> Preterm a
  PreConjunct :: Preterm a -> Preterm a -> Preterm a
  PreDisjunct :: Preterm a -> Preterm a -> Preterm a
  PreComplement :: Preterm a -> Preterm a
  PreAlways :: Maybe SrcLoc -> Preterm a -> Preterm a
  PreUpUntil :: Maybe SrcLoc -> Preterm a -> Preterm a -> Preterm a
  PreEventually :: Maybe SrcLoc -> Preterm a -> Preterm a
  deriving (Functor, Eq)

-- | @since 0.1.0.0
instance Show a => Show (Preterm a) where
  show = \case
    PreConst x -> show x
    PreConjunct x y -> "(" ++ show x ++ " /\\ " ++ show y ++ ")"
    PreDisjunct x y -> "(" ++ show x ++ " \\/ " ++ show y ++ ")"
    PreComplement x -> "not " ++ show x
    PreAlways loc x -> "always[" ++ show loc ++ ":" ++ show x ++ "]"
    PreUpUntil loc x y -> "until[" ++ show loc ++ ":" ++ show x ++ "][" ++ show y ++ "]"
    PreEventually loc x -> "eventually[" ++ show loc ++ ":" ++ show x ++ "]"

-- ---------------------------------------------------------------------------------------------------------------------

-- | This data type represents rules on 'Preterm'. 'Rewrite' wraps a 'Preterm' transformation that can result in
-- @'Just' t@ if @t@ is the antecedent of a successful rewrite or 'Nothing' if the succeding pattern failed to match.
--
-- @since 0.1.0.0
newtype Rewrite a = Rewrite
  {runRewrite :: Preterm a -> Maybe (Preterm a)}

-- | @f '<>' g@ on 'Rewrite' joins the rules @f@ and @g@ under the 'Alternative'. The sum @r_1 '<>' ... '<>' r_n@ tries
-- each rule from left-to-right to yield 'Just' of the first successful application or 'Nothing'.
--
-- @since 0.1.0.0
instance Semigroup (Rewrite a) where
  f <> g = Rewrite \term -> runRewrite f term <|> runRewrite g term
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid (Rewrite a) where
  mempty = Rewrite (const Nothing)
  {-# INLINE mempty #-}

-- | Takes a 'Preterm' expression to an equivalent expression in disjunctive normal form.
--
-- @since 0.1.0.0
normalForm :: Preterm a -> Preterm a
normalForm term = case normalize term of
  (False, term') -> term'
  (True, term') -> normalForm term'
  where
    normalize :: Preterm a -> (Bool, Preterm a)
    normalize e = case distributeRewrites e of
      (False, e') -> applyRewrite e'
      (True, e') -> (True, normalForm e')

-- | Descends a 'Preterm' expression, normalizing each term in the syntax tree as far as possible through repeated
-- applications of the rewrite rules in 'redexAlternatives'.
--
-- @since 0.1.0.0
distributeRewrites :: Preterm a -> (Bool, Preterm a)
distributeRewrites = \case
  PreConst x -> (False, PreConst x)
  PreConjunct e1 e2 ->
    let (t1, e1') = applyRewrite e1
        (t2, e2') = applyRewrite e2
     in (t1 || t2, PreConjunct e1' e2')
  PreDisjunct e1 e2 ->
    let (t1, e1') = applyRewrite e1
        (t2, e2') = applyRewrite e2
     in (t1 || t2, PreDisjunct e1' e2')
  PreComplement e ->
    let (t, e') = applyRewrite e
     in (t, PreComplement e')
  PreAlways loc e ->
    let (t, e') = applyRewrite e
     in (t, PreAlways loc e')
  PreUpUntil loc e1 e2 ->
    let (t1, e1') = applyRewrite e1
        (t2, e2') = applyRewrite e2
     in (t1 || t2, PreUpUntil loc e1' e2')
  PreEventually loc e ->
    let (t, e') = applyRewrite e
     in (t, PreEventually loc e')
{-# INLINE distributeRewrites #-}

-- | Attempts to reduce the given 'Preterm' by any one of the rules in 'redexAlternatives'. If @'applyRewrite' t@ yields
-- @(b, t')@, then @t'@ is equal to @t@ or some new term given by rewriting @t@ with a rule in 'redexAlternatives'.
-- @b :: 'Bool'@ indicates whether transformation took place or not, i.e. @b ≡ t /= t'@.
--
-- @since 0.1.0.0
applyRewrite :: Preterm a -> (Bool, Preterm a)
applyRewrite term = case runRewrite redexAlternatives term of
  Nothing -> (False, term)
  Just term' -> (True, term')
{-# INLINE applyRewrite #-}

-- | 'redexAlternatives' is a sum of the individual reductions on 'Preterm' under 'Alternative'.
--
-- @since 0.1.0.0
redexAlternatives :: Rewrite a
redexAlternatives =
  redexNegInvolute
    <> redexAlwaysDual
    <> redexAlwaysFactorsAnd
    <> redexEventuallyDual
    <> redexNegDistribAnd
    <> redexNegDistribOr
    <> redexEventuallyIdempotent
    <> redexEventuallyDistribOr
    <> redexEventuallyAbsorbs
    <> redexAlwaysIdempotent
    <> redexAlwaysAbsorbs
    <> redexAndDistribOrLeft
    <> redexAndDistribOrRight
{-# INLINE redexAlternatives #-}

-- ---------------------------------------------------------------------------------------------------------------------

-- | Negation is involutory.
--
-- @
-- ¬¬p ≡ p
-- @
--
-- @since 0.1.0.0
redexNegInvolute :: Rewrite a
redexNegInvolute = Rewrite \case
  PreComplement (PreComplement term) -> Just term
  _ -> Nothing
{-# INLINE redexNegInvolute #-}

-- | The dual of always is eventually.
--
-- @
-- ¬◻p ≡ ◊¬p
-- @
--
-- @since 0.1.0.0
redexAlwaysDual :: Rewrite a
redexAlwaysDual = Rewrite \case
  PreComplement (PreAlways loc term) -> return (PreEventually loc (PreComplement term))
  _ -> Nothing
{-# INLINE redexAlwaysDual #-}

-- | Always factors over conjunction.
--
-- @
-- ◻p ∧ ◻q ≡ ◻(p ∧ q)
-- @
--
-- @since 0.1.0.0
redexAlwaysFactorsAnd :: Rewrite a
redexAlwaysFactorsAnd = Rewrite \case
  PreConjunct (PreAlways loc1 left) (PreAlways _ right) -> return (PreAlways loc1 (PreConjunct left right))
  _ -> Nothing
{-# INLINE redexAlwaysFactorsAnd #-}

-- | The dual of eventually is always.
--
-- @
-- ¬◊p ≡ ◻¬p
-- @
--
-- @since 0.1.0.0
redexEventuallyDual :: Rewrite a
redexEventuallyDual = Rewrite \case
  PreComplement (PreEventually loc term) -> return (PreAlways loc (PreComplement term))
  _ -> Nothing
{-# INLINE redexEventuallyDual #-}

-- | Negation distributes over conjunction.
--
-- @
-- ¬(a ∧ b) ≡ ¬a ∨ ¬b
-- @
--
-- @since 0.1.0.0
redexNegDistribAnd :: Rewrite a
redexNegDistribAnd = Rewrite \case
  PreComplement (PreConjunct left right) -> return (PreDisjunct (PreComplement left) (PreComplement right))
  _ -> Nothing
{-# INLINE redexNegDistribAnd #-}

-- | Negation distributes over disjunction.
--
-- @
-- ¬(a ∨ b) ≡ ¬a ∧ ¬b
-- @
--
-- @since 0.1.0.0
redexNegDistribOr :: Rewrite a
redexNegDistribOr = Rewrite \case
  PreComplement (PreDisjunct left right) -> do
    return (PreConjunct (PreComplement left) (PreComplement right))
  _ -> Nothing
{-# INLINE redexNegDistribOr #-}

-- | Eventually is idempotent.
--
-- @
-- ◊◊p ≡ ◊p
-- @
--
-- @since 0.1.0.0
redexEventuallyIdempotent :: Rewrite a
redexEventuallyIdempotent = Rewrite \case
  PreEventually loc1 (PreEventually _ term) -> return (PreEventually loc1 term)
  _ -> Nothing
{-# INLINE redexEventuallyIdempotent #-}

-- | Eventually distributes over or.
--
-- @
-- ◊(p ∨ q) ≡ ◊p ∨ ◊q
-- @
--
-- @since 0.1.0.0
redexEventuallyDistribOr :: Rewrite a
redexEventuallyDistribOr = Rewrite \case
  PreEventually loc (PreDisjunct left right) -> return (PreDisjunct (PreEventually loc left) (PreEventually loc right))
  _ -> Nothing
{-# INLINE redexEventuallyDistribOr #-}

-- | Eventually is absorbed by always-eventually.
--
-- @
-- ◊◻◊p ≡ ◻◊p
-- @
--
-- @since 0.1.0.0
redexEventuallyAbsorbs :: Rewrite a
redexEventuallyAbsorbs = Rewrite \case
  PreEventually _ (PreAlways loc1 (PreEventually loc2 term)) -> return (PreAlways loc1 (PreEventually loc2 term))
  _ -> Nothing
{-# INLINE redexEventuallyAbsorbs #-}

-- | Always is idempotent.
--
-- @
-- ◻◻p ≡ ◻p
-- @
--
-- @since 0.1.0.0
redexAlwaysIdempotent :: Rewrite a
redexAlwaysIdempotent = Rewrite \case
  PreAlways loc1 (PreAlways _ term) -> Just (PreAlways loc1 term)
  _ -> Nothing
{-# INLINE redexAlwaysIdempotent #-}

-- | Always is absorbed by eventually-always.
--
-- @
-- ◻◊◻p ≡ ◊◻p
-- @
--
-- @since 0.1.0.0
redexAlwaysAbsorbs :: Rewrite a
redexAlwaysAbsorbs = Rewrite \case
  PreAlways _ (PreEventually loc1 (PreAlways loc2 term)) -> return (PreEventually loc1 (PreAlways loc2 term))
  _ -> Nothing
{-# INLINE redexAlwaysAbsorbs #-}

-- | Conjunction left-distributes over disjunction.
--
-- @
-- a ∧ (b ∨ c) ≡ (a ∧ b) ∨ (a ∧ c)
-- @
--
-- @since 0.1.0.0
redexAndDistribOrLeft :: Rewrite a
redexAndDistribOrLeft = Rewrite \case
  PreConjunct a (PreDisjunct b c) -> return (PreDisjunct (PreConjunct a b) (PreConjunct a c))
  _ -> Nothing
{-# INLINE redexAndDistribOrLeft #-}

-- | Conjunction right-distributes over disjunction.
--
-- @
-- (a ∨ b) ∧ c ≡ (a ∧ c) ∨ (b ∧ c)
-- @
--
-- @since 0.1.0.0
redexAndDistribOrRight :: Rewrite a
redexAndDistribOrRight = Rewrite \case
  PreConjunct (PreDisjunct a b) c -> return (PreDisjunct (PreConjunct a c) (PreConjunct b c))
  _ -> Nothing
{-# INLINE redexAndDistribOrRight #-}
