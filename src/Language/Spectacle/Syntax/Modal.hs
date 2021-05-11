--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Language.Spectacle.Syntax.Modal where

--   ( -- * Labels
--     Modal (Modal),
--     Effect (Always, UpUntil),

--     -- * Syntax
--     always,
--     eventually,
--     upUntil,

--     -- * Interpreters

--     -- runFormula,
--     applyModality,
--     introducePrimes,

--     -- * Quoting

--     -- ModalQ (ConstQ, NextQ, ModalQ),
--     -- ModalMeta
--     --   ( MetaAlways,
--     --     MetaUpUntil,
--     --     MetaForall,
--     --     MetaExists,
--     --     MetaComplement,
--     --     MetaConjunct,
--     --     MetaDisjunct
--     --   ),
--     -- lowerQuoted,

--     -- ** Constructors

--     -- freshAlwaysQ,
--     -- freshUpUntilQ,
--     -- freshEventuallyQ,
--     -- freshComplementQ,
--     -- freshConjunctQ,
--     -- freshDisjunctQ,
--     -- normalize,
--     -- normalForm,
--   )
-- where

import Control.Monad.Trans.Reader
import Data.Function ((&))

import Data.Functor.Loom
import Data.Type.Rec
import Language.Spectacle.Exception.RuntimeException
import Language.Spectacle.Lang
import Language.Spectacle.Syntax.Error
import Language.Spectacle.Syntax.Logic.Internal
import Language.Spectacle.Syntax.Modal.Internal (Effect (Always, UpUntil), Modal)
import Language.Spectacle.Syntax.Modal.Preterm
import Language.Spectacle.Syntax.NonDet
import Language.Spectacle.Syntax.Plain (Effect (PlainVar), runPlain)
import Language.Spectacle.Syntax.Plain.Internal (Plain)
import Language.Spectacle.Syntax.Prime (Effect (PrimeVar), Prime, substPrime)

-- ---------------------------------------------------------------------------------------------------------------------

-- | The modal operator 'always' qualifies a formula @p@ such that for all actions taken in a specification, the formula
-- @p@ must be true.
--
-- [Valid]
-- @
--                              ... ───── * ───── * ───── * ───── * ───── * ───── ...
--                                        p       p       p       p       p
-- @
--
-- [Invalid]
-- @
--                              ... ───── * ───── * ───── * ───── * ───── * ───── ...
--                                        p       p       p               p
-- @
--
-- @since 0.1.0.0
always :: Member Modal effs => Lang ctx effs Bool -> Lang ctx effs Bool
always m = scope (Always m)
{-# INLINE always #-}

-- | The modal operator 'eventually' qualifies a formula @p@ such that @p@ must be true /now/, or some time in the
-- future. 'eventually' is equivalent to 'upUntil' with a trivial formula on the left-hand side:
--
-- @
-- 'eventually' p = 'upUntil' ('pure' 'True') p
-- @
--
-- [Valid]
-- @
--                              ... ───── * ───── * ───── * ───── * ───── * ───── ...
--                                                                p
--
--                              ... ───── * ───── * ───── * ───── * ───── * ───── ...
--                                                        p               p
--
--                              ... ───── * ───── * ───── * ───── * ───── * ───── ...
--                                        p       p       p       p       p
-- @
--
-- [Invalid]
-- @
--                              ... ───── * ───── * ───── * ───── * ───── * ───── ...
-- @
--
-- @since 0.1.0.0
eventually :: Member Modal effs => Lang ctx effs Bool -> Lang ctx effs Bool
eventually = upUntil (pure True)

-- | The modal operator 'upUntil' (strong) says that for some formula @p `upUntil` q@, @p@ must be true up until the
-- point where @q@ is true, and that @q@ /must/ be true some time in the future.
--
-- [Valid]
-- @
--                              ... ───── * ───── * ───── * ───── * ───── * ───── ...
--
--                                        p       p       q
--
--                              ... ───── * ───── * ───── * ───── * ───── * ───── ... \n
--
--                                     (p ∧ q)
--
--                              ... ───── * ───── * ───── * ───── * ───── * ───── ...
--                                        p       q               p       q
-- @
--
-- [Invalid]
-- @
--                              ... ───── * ───── * ───── * ───── * ───── * ───── ...
--                                        p       p       p               q
--
--                              ... ───── * ───── * ───── * ───── * ───── * ───── ...
--                                        p       p       p       p       p
--
--                              ... ───── * ───── * ───── * ───── * ───── * ───── ...
-- @
--
-- @since 0.1.0.0
upUntil :: Member Modal effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
upUntil m n = scope (UpUntil m n)

data RedexModality
  = RedexAlways
  | RedexEventually
  | RedexNone

data Expanded a = Expanded
  { reduced :: a
  , newFormula :: Preterm a
  }

type FormulaSyntax =
  '[ Modal
   , Logic
   , Prime
   , Plain
   , NonDet
   , Error RuntimeException
   ]

getPreterms ::
  Rec ctx ->
  Rec ctx ->
  Lang ctx '[Prime, Plain, NonDet, Error RuntimeException] a ->
  Either RuntimeException [a]
getPreterms vals vals' effs =
  effs
    & substPrime vals'
    & runPlain vals
    & runNonDetA
    & runError
    & runLang

commit :: Lang ctx effs (Expanded a) -> (Lang ctx effs a, Lang ctx effs (Preterm a))
commit effs = (fmap reduced effs, fmap newFormula effs)

expand :: Preterm Bool -> Reader RedexModality (Expanded Bool)
expand = \case
  PreConst x -> return (Expanded x (PreConst x))
  PreConjunct lhs rhs -> do
    redex <- ask
    lhs' <- fmap reduced (expand lhs)
    rhs' <- fmap reduced (expand rhs)
    if
        | True <- lhs'
          , True <- rhs' -> case redex of
          -- In the scope of an enclosing 'Always' operator, we need to check that the reduced temporal formula
          -- results in @'PreConst' 'True'@ (i.e. the atomic proposition qualified by 'Always' is satisfied).
          -- However, since we need to continue to check that this proposition is true by the definition of 'Always',
          -- we do not give back different formula since the reduction semantics over a "temporal action" (relation)
          -- is
          --
          -- A p -> p /\ Next (A p)
          --
          -- The next operator is not directly accessible in TLA+ so we just give back the original formula in the
          -- expanded form of always.
          RedexAlways -> return (Expanded True (PreConjunct lhs rhs))
          -- For an enclosing always, we know that the precondition for Until operator has been satisfied, so we can
          -- reduce all atomic propositions as much as possible. This is given by the reduction semantics for
          -- eventually:
          --
          -- True U p -> p \/ Next (True U p)
          --
          -- Since p is true in the disjunction here we can discard the RHS and give back a reduced formula.
          RedexEventually -> return (Expanded True (PreConst True))
          -- For atomic proposition not enclosed in either a 'Always' or 'Eventually' we can reduce.
          RedexNone -> return (Expanded True (PreConst True))
        | otherwise -> case redex of
          -- Witnessing false in a conjunct enclosed by an Always operator is an error.
          --
          -- TODO: This is not always true, if this conjunction is on either side of a disjunction this isn't a
          -- problem, soft failure here probably solves this problem given the reduction semantics for disjunction.
          RedexAlways -> error "LHS conjunct enclosed in always was (PreConst False)"
          -- For any atomic proposition conjoined to a false proposition cannot be reduced enclosed in eventually. You
          -- might think that it can; however, since eventually does not distribute over conjunction reducing the LHS
          -- here would result in a semantically different proposition.
          RedexEventually -> return (Expanded False (PreConjunct lhs rhs))
          -- We can reduce a False conjoined to any proposition to just False when not qualified by any modal operator.
          RedexNone -> return (Expanded False (PreConjunct lhs rhs))
  PreDisjunct lhs rhs -> do
    redex <- ask
    lhs' <- fmap reduced (expand lhs)
    rhs' <- fmap reduced (expand rhs)
    if
        | True <- lhs' -> case redex of
          RedexAlways -> return (Expanded True (PreDisjunct lhs rhs))
          RedexEventually -> return (Expanded True (PreConst True))
          RedexNone -> return (Expanded True (PreConst True))
        | True <- rhs' -> case redex of
          RedexAlways -> return (Expanded True (PreDisjunct lhs rhs))
          RedexEventually -> return (Expanded True (PreConst True))
          RedexNone -> return (Expanded True (PreConst True))
        | otherwise -> case redex of
          RedexAlways -> error "One side of disjunction enclosed in always was (PreConst False)"
          RedexEventually -> return (Expanded False (PreDisjunct lhs rhs))
          RedexNone -> return (Expanded False (PreConst False))
  PreComplement expr -> do
    Expanded x expr' <- expand expr
    return (Expanded (not x) (PreComplement expr'))
  PreAlways expr -> do
    Expanded x _ <- local (const RedexAlways) (expand expr)
    return (Expanded x (PreAlways expr))
  PreUpUntil lhs rhs ->
    expand lhs >>= \case
      Expanded True _ ->
        local (const RedexEventually) (expand rhs) >>= \case
          Expanded False _ -> return (Expanded True (PreUpUntil lhs rhs))
          Expanded True _ -> return (Expanded True (PreConst True))
      Expanded False _ -> error "eventually violated"

introducePrimes :: Lang ctx (Modal ': Logic ': effs) a -> Lang ctx (Modal ': Logic ': Prime ': effs) a
introducePrimes = \case
  Pure x -> pure x
  Op op k -> Op (extendOp op) (introducePrimes . k)
  Scoped scoped loom -> Scoped (extendS scoped) (loom ~>~ hoist introducePrimes)
  where
    extendOp :: Op (Modal ': Logic ': effs) a -> Op (Modal ': Logic ': Prime ': effs) a
    extendOp = \case
      OHere op -> OHere op
      OThere (OHere op) -> OThere (OHere op)
      OThere (OThere op) -> OThere (OThere (OThere op))

    extendS :: Scoped (Modal ': Logic ': effs) m a -> Scoped (Modal ': Logic ': Prime ': effs) m a
    extendS = \case
      SHere scoped -> SHere scoped
      SThere (SHere scoped) -> SThere (SHere scoped)
      SThere (SThere scoped) -> SThere (SThere (SThere scoped))

applyModality :: Members '[Modal, Logic, Plain, Prime] effs => Lang ctx effs Bool -> Lang ctx effs Bool
applyModality = \case
  Pure x -> pure x
  Op op k -> Op op (applyModality . k)
  Scoped scoped loom -> case projectS scoped of
    Nothing -> Scoped scoped loom'
    Just (Always expr) -> do
      let expr' = runLoom loom' expr
      scope (Always expr')
    Just (UpUntil lhs rhs) -> do
      let lhs' = runLoom loom' lhs
          rhs' = runLoom (loom' ~>~ hoist replacePrimes) rhs
      scope (UpUntil lhs' rhs')
    where
      loom' = loom ~>~ hoist applyModality
  where
    replacePrimes :: Members '[Plain, Prime] effs => Lang ctx effs a -> Lang ctx effs a
    replacePrimes = \case
      Pure x -> pure x
      Op op k -> Op op (replacePrimes . k)
      Scoped scoped loom -> case projectS scoped of
        Nothing -> Scoped scoped loom'
        Just (PlainVar name) -> Scoped (injectS (PrimeVar name)) loom'
        where
          loom' = loom ~>~ hoist replacePrimes
