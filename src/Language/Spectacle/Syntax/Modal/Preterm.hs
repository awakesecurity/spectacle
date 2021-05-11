-- | This module houses 'Preterm's and their supporting functions. 'Preterm's serve as an intermediate representation
-- for manipulating temporal formula and are isomorphic to their effectful representation in the 'Modal' and 'Logic'
-- effects.
--
-- Rewrite rules are applied to 'Preterm's to produce a definitonally equivalent formula whose modal operators have
-- "pushed" up the syntax tree and logical operators "pushed" down, or factored out entirely. Normalizing an expression
-- this way lets us avoid having to deduce which modality to use in the model checker as it is immediately apparent at
-- the root of a formula's AST. An example of the reduction process for the formula such as
--
-- @
-- (□ p₁ ∧ □ p₂) ∧ ¬ (◇ p₃ ∨ ◇ p₄) ≡ □ (p₁ ∧ p₂ ∧ p₃ ∧ p₄)
-- @
--
-- would be:
--
-- @
--             (∧)
--              │                       (∧)                     (□)
--        ╭─────┴─────╮                  │                       │
--       (∨)         (¬)   ====>    ╭────┴────╮     ====>       (∧)
--        │           │            (∧)       (∧)           ╭─────┴─────╮
--     ╭──┴──╮        │          ╭──┴──╮   ╭──┴──╮        (∧)         (∧)
--    (□)   (□)      (∨)        (□)   (□) (□)   (□)     ╭──┴──╮     ╭──┴──╮
--     │     │     ╭──┴──╮       │     │   │     │     p₁     p₂   p₃     p₄
--     p₁    p₂   (◇)   (◇)      p₁    p₂  p₃    p₄
--                 │     │
--                 p₂    p₄
-- @
--
--
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Modal.Preterm
  ( Preterm (PreConst, PreConjunct, PreDisjunct, PreComplement, PreAlways, PreUpUntil),
    materialize,
    abstract,
    normalizePreterm,
    rewritePreterm,
  )
where

import Data.Function ((&))
import Data.Void (absurd)

import Data.Functor.Loom (hoist, runLoom, (~>~))
import Language.Spectacle.Lang
  ( Lang (Pure, Op, Scoped),
    Member,
    Members,
    decomposeOp,
    decomposeS,
    scope,
    weaken,
  )
import Language.Spectacle.Syntax.Logic.Internal (Effect (Complement, Conjunct, Disjunct), Logic (Logic))
import Language.Spectacle.Syntax.Modal.Internal (Effect (Always, UpUntil), Modal (Modal))
import Language.Spectacle.Syntax.NonDet.Internal (NonDet)

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'Preterm' is the raw underlying syntax tree representing a temporal formula modulo the continuation or
-- 'Data.Functor.Loom' information. A tree of 'Preterm's can be built by temporarily dispatching the 'Modal' and 'Logic'
-- effects with 'materialize' and converted back into their effectful representation with 'abstract', which forms an
-- isomorphism:
--
-- @
--                                                      'materialize'
--                                   ───────────────────────────────────────────────>
-- Members '[Modal, Logic] effs =>                                                      Preterm Bool
--   Lang ctx effs Bool              <───────────────────────────────────────────────
--                                                       'abstract'
-- @
--
-- 'Preterm's are rewritten in the normalization process rather than the effects directly since interposing effect
-- translations directly would be extremely complicated for the set of tautologies used to normalize temporal formula.
--
-- @since 0.1.0.0
data Preterm a where
  PreConst :: a -> Preterm a
  PreConjunct :: Preterm a -> Preterm a -> Preterm a
  PreDisjunct :: Preterm a -> Preterm a -> Preterm a
  PreComplement :: Preterm a -> Preterm a
  PreAlways :: Preterm a -> Preterm a
  PreUpUntil :: Preterm a -> Preterm a -> Preterm a
  deriving (Functor, Eq, Show)

-- | 'materialize' takes a 'Lang' with 'Modal' and 'Logic' effects and converts their representation to 'Preterm's.
--
-- @since 0.1.0.0
materialize :: Lang ctx (Modal ': Logic ': effs) a -> Lang ctx effs (Preterm a)
materialize = \case
  Pure x -> pure (PreConst x)
  Op op k -> case decomposeOp op of
    Left op' -> case decomposeOp op' of
      Left other -> Op other (materialize . k)
      Right (Logic b) -> absurd b
    Right (Modal b) -> absurd b
  Scoped scoped loom -> case decomposeS scoped of
    Left scoped' -> case decomposeS scoped' of
      Left other -> Scoped other loom'
      Right eff
        | Conjunct lhs rhs <- eff -> do
          lhs' <- runLoom loom' lhs
          rhs' <- runLoom loom' rhs
          return (PreConjunct lhs' rhs')
        | Disjunct lhs rhs <- eff -> do
          lhs' <- runLoom loom' lhs
          rhs' <- runLoom loom' rhs
          return (PreDisjunct lhs' rhs')
        | Complement expr <- eff -> do
          expr' <- runLoom loom' expr
          return (PreComplement expr')
    Right eff
      | Always expr <- eff -> do
        expr' <- runLoom loom' expr
        return (PreAlways expr')
      | UpUntil lhs rhs <- eff -> do
        lhs' <- runLoom loom' lhs
        rhs' <- runLoom loom' rhs
        return (PreUpUntil lhs' rhs')
    where
      loom' = loom ~>~ hoist materialize

-- | 'abstract' reintroduces the 'Preterm's in a 'Lang' of 'Preterms' as their corresponding effects.
--
-- @since 0.1.0.0
abstract :: Member NonDet effs => Lang ctx effs (Preterm Bool) -> Lang ctx (Modal ': Logic ': effs) Bool
abstract preterms =
  preterms
    & weaken
    & weaken
    & (>>= fromPreterm)
  where
    fromPreterm :: Members '[Modal, Logic, NonDet] effs => Preterm Bool -> Lang ctx effs Bool
    fromPreterm = \case
      PreConst x -> Pure x
      PreConjunct lhs rhs -> scope (Conjunct (fromPreterm lhs) (fromPreterm rhs))
      PreDisjunct lhs rhs -> scope (Disjunct (fromPreterm lhs) (fromPreterm rhs))
      PreComplement expr -> scope (Complement (fromPreterm expr))
      PreUpUntil lhs rhs -> scope (UpUntil (fromPreterm lhs) (fromPreterm rhs))
      PreAlways expr -> scope (Always (fromPreterm expr))
{-# INLINE abstract #-}

-- | Sends a tree of 'Preterm's to a normal form which can be easily recognized by the model checker.
--
-- @since 0.1.0.0
normalizePreterm :: Preterm Bool -> Preterm Bool
normalizePreterm preterm =
  let preterm' = rewritePreterm preterm
   in if preterm' == preterm
        then preterm'
        else preterm

-- | Simplifies temporal formula by rewriting a tree of preterms using tautologies for the modal operators:
--
-- [Always/eventually distributes]
--
-- @
-- conjunct (always p) (always q) = always (conjunct p q)
-- disjunct (eventually p) (eventually q) = eventually (disjunct p q)
-- @
--
-- [Always/eventually involutes]
--
-- @
-- always (always p) = always p
-- eventually (eventually p) = eventually p
-- @
--
-- [Always/eventually absorption]
--
-- @
-- always (eventually (always p)) = eventually (always p)
-- eventually (always (eventually p)) = always (eventually p)
-- @
--
-- [Until is eventually]
--
-- @
-- upUntil (pure True) p = eventually p
-- @
--
-- [Always/eventually dual]
--
-- @
-- complement (always p) = eventually (complement p)
-- complement (eventually p) = always (complement p)
-- @
--
-- [Negation involutes]
--
-- @
-- complement (complement p) = p
-- @
--
-- [Negation distributes]
--
-- @
-- complement (conjunct p q) = disjunct (complement p) (complement q)
-- complement (disjunct p q) = conjunct (complement p) (complement q)
-- @
--
-- @since 0.1.0.0
rewritePreterm :: Preterm Bool -> Preterm Bool
rewritePreterm = \case
  PreConst x -> PreConst x
  PreConjunct lhs rhs
    | PreAlways lhs' <- lhs
      , PreAlways rhs' <- rhs ->
      rewritePreterm (PreAlways (PreConjunct lhs' rhs'))
    | otherwise -> PreConjunct (rewritePreterm lhs) (rewritePreterm rhs)
  PreDisjunct lhs rhs
    | PreUpUntil (PreConst True) lhs' <- lhs
      , PreUpUntil (PreConst True) rhs' <- rhs ->
      rewritePreterm (PreUpUntil (PreConst True) (PreDisjunct lhs' rhs'))
    | otherwise -> PreDisjunct (rewritePreterm lhs) (rewritePreterm rhs)
  PreComplement expr
    | PreComplement expr' <- expr ->
      rewritePreterm expr'
    | PreConjunct lhs rhs <- expr ->
      let lhs' = rewritePreterm (PreComplement lhs)
          rhs' = rewritePreterm (PreComplement rhs)
       in PreDisjunct lhs' rhs'
    | PreDisjunct lhs rhs <- expr ->
      let lhs' = rewritePreterm (PreComplement lhs)
          rhs' = rewritePreterm (PreComplement rhs)
       in PreConjunct lhs' rhs'
    | PreAlways expr' <- expr ->
      PreUpUntil (PreConst True) (rewritePreterm (PreComplement expr'))
    | PreUpUntil (PreConst True) expr' <- expr ->
      PreAlways (rewritePreterm (PreComplement expr'))
    | otherwise -> PreComplement (rewritePreterm expr)
  PreAlways expr
    | PreAlways expr' <- expr ->
      rewritePreterm (PreAlways expr')
    | PreUpUntil (PreConst True) expr' <- expr
      , PreAlways {} <- expr' ->
      rewritePreterm (PreUpUntil (PreConst True) expr')
    | otherwise -> PreAlways (rewritePreterm expr)
  PreUpUntil (PreConst True) expr
    | PreAlways expr' <- expr
      , PreUpUntil (PreConst True) _ <- expr' ->
      rewritePreterm (PreAlways expr')
    | expr'@(PreUpUntil (PreConst True) _) <- expr -> rewritePreterm expr'
    | otherwise -> PreUpUntil (PreConst True) (rewritePreterm expr)
  PreUpUntil lhs rhs -> PreUpUntil (rewritePreterm lhs) (rewritePreterm rhs)
{-# INLINE rewritePreterm #-}
