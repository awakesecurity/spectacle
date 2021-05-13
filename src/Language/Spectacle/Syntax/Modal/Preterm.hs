{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  ( Preterm
      ( PreConst,
        PreConjunct,
        PreDisjunct,
        PreImplies,
        PreNotImplies,
        PreComplement,
        PreAlways,
        PreUpUntil
      ),
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
  ( Lang (Op, Pure, Scoped),
    Members,
    decomposeOp,
    decomposeS,
    scope,
    weaken,
  )
import Language.Spectacle.Syntax.Logic.Internal (Effect (Complement, Conjunct, Disjunct), Logic (Logic))
import Language.Spectacle.Syntax.Modal.Internal (Effect (Always, UpUntil), Modal (Modal))

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
-- translations directly would be extremely complicated for the set of tautologies used to normalize temporal formulas.
--
-- @since 0.1.0.0
data Preterm a where
  PreConst :: a -> Preterm a
  PreConjunct :: Preterm a -> Preterm a -> Preterm a
  PreDisjunct :: Preterm a -> Preterm a -> Preterm a
  PreImplies :: Preterm a -> Preterm a -> Preterm a
  PreNotImplies :: Preterm a -> Preterm a -> Preterm a
  PreComplement :: Preterm a -> Preterm a
  PreAlways :: Preterm a -> Preterm a
  PreUpUntil :: Preterm a -> Preterm a -> Preterm a
  deriving (Functor, Eq, Show)

isComplement :: Preterm a -> Bool
isComplement PreComplement {} = True
isComplement _ = False
{-# INLINE CONLIKE isComplement #-}

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
      Left other -> Scoped other loomReify
      Right eff
        | Conjunct lhs rhs <- eff -> do
          lhs' <- runLoom loomReify lhs
          rhs' <- runLoom loomReify rhs
          return (PreConjunct lhs' rhs')
        | Disjunct lhs rhs <- eff -> do
           runLoom loomReify lhs >>= \case
            PreComplement lhs' -> do
              rhs' <- runLoom loomReify rhs
              if isComplement rhs'
                then return (PreDisjunct lhs' rhs')
                else return (PreImplies lhs' rhs')
            lhs' -> do
              rhs' <- runLoom loomReify rhs
              return (PreDisjunct lhs' rhs')
        | Complement expr <- eff -> do
          expr' <- runLoom loomReify expr
          return (PreComplement expr')
    Right eff
      | Always expr <- eff -> do
        expr' <- runLoom loomReify expr
        return (PreAlways expr')
      | UpUntil lhs rhs <- eff -> do
        lhs' <- runLoom loomReify lhs
        rhs' <- runLoom loomReify rhs
        return (PreUpUntil lhs' rhs')
    where
      loomReify = loom ~>~ hoist materialize

-- | 'abstract' reintroduces the 'Preterm's in a 'Lang' of 'Preterms' as their corresponding effects.
--
-- @since 0.1.0.0
abstract :: Lang ctx effs (Preterm Bool) -> Lang ctx (Modal ': Logic ': effs) Bool
abstract preterms =
  preterms
    & weaken
    & weaken
    & (>>= fromPreterm)
  where
    fromPreterm :: Members '[Modal, Logic] effs => Preterm Bool -> Lang ctx effs Bool
    fromPreterm = \case
      PreConst x -> Pure x
      PreConjunct lhs rhs ->
        let lhs' = fromPreterm lhs
            rhs' = fromPreterm rhs
         in scope (Conjunct lhs' rhs')
      PreDisjunct lhs rhs ->
        let lhs' = fromPreterm lhs
            rhs' = fromPreterm rhs
         in scope (Disjunct lhs' rhs')
      PreImplies lhs rhs ->
        let lhs' = scope (Complement (fromPreterm lhs))
            rhs' = fromPreterm rhs
         in scope (Disjunct lhs' rhs')
      PreNotImplies lhs rhs ->
        let lhs' = fromPreterm lhs
            rhs' = scope (Complement (fromPreterm rhs))
         in scope (Conjunct lhs' rhs')
      PreComplement term ->
        let term' = fromPreterm term
         in scope (Complement term')
      PreUpUntil lhs rhs ->
        let lhs' = fromPreterm lhs
            rhs' = fromPreterm rhs
         in scope (UpUntil lhs' rhs')
      PreAlways term ->
        let term' = fromPreterm term
         in scope (Always term')
{-# INLINE abstract #-}

-- | Sends a tree of 'Preterm's to a normal form which can be easily recognized by the model checker.
--
-- @since 0.1.0.0
normalizePreterm :: Preterm Bool -> Preterm Bool
normalizePreterm preterm =
  let preterm' = rewritePreterm preterm
   in if preterm' == preterm
        then preterm'
        else rewritePreterm preterm'

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
      -- ◻p ∧ ◻q ≡ ◻(p ∧ q)
      rewritePreterm (PreAlways (PreConjunct lhs' rhs'))
    | otherwise -> PreConjunct (rewritePreterm lhs) (rewritePreterm rhs)
  PreDisjunct lhs rhs
    | PreUpUntil (PreConst True) lhs' <- lhs
      , PreUpUntil (PreConst True) rhs' <- rhs ->
      -- ◇ p ∨ ◇ q ≡ ◇(p ∨ q)
      rewritePreterm (PreUpUntil (PreConst True) (PreDisjunct lhs' rhs'))
    | otherwise -> PreDisjunct (rewritePreterm lhs) (rewritePreterm rhs)
  PreImplies lhs rhs ->
    let lhs' = rewritePreterm lhs
        rhs' = rewritePreterm rhs
     in PreImplies lhs' rhs'
  PreNotImplies lhs rhs ->
    let lhs' = rewritePreterm lhs
        rhs' = rewritePreterm rhs
     in PreNotImplies lhs' rhs'
  PreComplement expr
    | PreComplement expr' <- expr ->
      -- ¬ (¬ p) ≡ p
      rewritePreterm expr'
    | PreConjunct lhs rhs <- expr ->
      -- ¬(p ∧ q) ≡ ¬ p ∨ ¬ q
      let lhs' = rewritePreterm (PreComplement lhs)
          rhs' = rewritePreterm (PreComplement rhs)
       in PreDisjunct lhs' rhs'
    | PreDisjunct lhs rhs <- expr ->
      -- ¬(p ∨ q) ≡ ¬ p ∧ ¬ q
      let lhs' = rewritePreterm (PreComplement lhs)
          rhs' = rewritePreterm (PreComplement rhs)
       in PreConjunct lhs' rhs'
    | PreImplies lhs rhs <- expr ->
      -- ¬(p ⇒ q) ≡ ¬(¬p ∧ q) ≡ ¬¬p ∨ ¬q ≡ p ∨ ¬q ≡ p ⇏ q
      PreNotImplies lhs rhs
    | PreNotImplies lhs rhs <- expr ->
      -- ¬(p ⇏ q) ≡ ¬(p ∨ ¬ q) ≡ ¬p ∧ ¬¬q ≡ ¬p ∧ q ≡ p ⇒ q
      PreImplies rhs lhs
    | PreAlways expr' <- expr ->
      -- ¬◻p ≡ ◇¬p
      PreUpUntil (PreConst True) (rewritePreterm (PreComplement expr'))
    | PreUpUntil (PreConst True) expr' <- expr ->
      -- ¬◇p ≡ ◻¬p
      PreAlways (rewritePreterm (PreComplement expr'))
    | otherwise -> PreComplement (rewritePreterm expr)
  PreAlways expr
    | PreAlways expr' <- expr ->
      -- ◻◻p ≡ ◻p
      rewritePreterm (PreAlways expr')
    | PreUpUntil (PreConst True) expr' <- expr
      , PreAlways {} <- expr' ->
      -- ◻◇◻p ≡ ◇◻p
      rewritePreterm expr
    | otherwise -> PreAlways (rewritePreterm expr)
  PreUpUntil (PreConst True) expr
    | PreAlways expr' <- expr
      , PreUpUntil (PreConst True) _ <- expr' ->
      -- ◇◻◇p ≡ ◻◇p
      rewritePreterm (PreAlways expr')
    | expr'@(PreUpUntil (PreConst True) _) <- expr ->
      -- ◇◇p ≡ ◇p
      rewritePreterm expr'
    | otherwise -> PreUpUntil (PreConst True) (rewritePreterm expr)
  PreUpUntil lhs rhs -> PreUpUntil (rewritePreterm lhs) (rewritePreterm rhs)
{-# INLINE rewritePreterm #-}
