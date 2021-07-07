{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Spectacle.Syntax.Modal.Term
  ( -- * Temporal Formula Term
    Term
      ( Value,
        Conjunct,
        Disjunct,
        Complement,
        Always,
        Eventually,
        StaysAs,
        InfinitelyOften,
        UpUntil
      ),
    termFromPreterm,

    -- * Name Supply Stack
    NameSupply (NameSupply, unNameSupply),
    runNameSupply,
    guardLevel,
    throwLevelMismatch,
  )
where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, when)
import Control.Monad.Fresh (Fresh, MonadFresh (fresh), evalFresh)
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT))
import Data.Function ((&))
import Data.Kind (Type)
import GHC.Stack (SrcLoc)

import Language.Spectacle.Exception.RuntimeException (SyntaxException (LevelMismatch))
import Language.Spectacle.Syntax.Modal.Level (ExprLevel (L2, L3, L4))
import Language.Spectacle.Syntax.Modal.Preterm
  ( Preterm
      ( PreAlways,
        PreComplement,
        PreConjunct,
        PreConst,
        PreDisjunct,
        PreEventually,
        PreUpUntil
      ),
  )

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'Term' is the syntax tree for temporal formula.
--
-- @since 0.1.0.0
data Term :: Type -> Type where
  Value :: a -> Term a
  Conjunct :: Term a -> Term a -> Term a
  Disjunct :: Term a -> Term a -> Term a
  Complement :: Term a -> Term a
  Always :: Maybe SrcLoc -> Int -> Term a -> Term a
  Eventually :: Maybe SrcLoc -> Int -> Term a -> Term a
  UpUntil :: Maybe SrcLoc -> Int -> Term a -> Term a -> Term a
  StaysAs :: Maybe SrcLoc -> Int -> Term a -> Term a
  InfinitelyOften :: Maybe SrcLoc -> Int -> Term a -> Term a

-- | @since 0.1.0.0
instance Show a => Show (Term a) where
  show = \case
    Value x -> show x ++ "\'"
    Conjunct e1 e2 -> "(" ++ show e1 ++ " ∧ " ++ show e2 ++ ")"
    Disjunct e1 e2 -> "(" ++ show e1 ++ " ∨ " ++ show e2 ++ ")"
    Complement e -> "¬" ++ show e
    Always _ n e -> "◻[" ++ show n ++ "]" ++ show e
    Eventually _ n e -> "◇[" ++ show n ++ "]" ++ show e
    UpUntil _ n e1 e2 -> show e1 ++ " U[" ++ show n ++ " " ++ show e2
    StaysAs _ n e -> "◇◻[" ++ show n ++ "]" ++ show e
    InfinitelyOften _ n e -> "◻◇[" ++ show n ++ "]" ++ show e
  {-# INLINE show #-}

-- | The 'NameSupply' monad stack is used to convert 'Preterm's to 'Term's which requires a supply of unique names that
-- identify expressions qualified by temporal operators and exception handling for malformed formula.
--
-- @since 0.1.0.0
newtype NameSupply a = NameSupply
  {unNameSupply :: ExceptT SyntaxException (ReaderT ExprLevel Fresh) a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( MonadError SyntaxException
    , MonadFresh
    , MonadReader ExprLevel
    )

-- | Run a 'NameSupply'.
--
-- @since 0.1.0.0
runNameSupply :: NameSupply a -> Either SyntaxException a
runNameSupply supply =
  supply
    & unNameSupply
    & runExceptT
    & flip runReaderT L4
    & evalFresh 0
{-# INLINE runNameSupply #-}

-- | Sends a 'Preterm' expression to an equivalent 'Term' expression.
--
-- @since 0.1.0.0
termFromPreterm :: Preterm a -> NameSupply (Term a)
termFromPreterm = \case
  PreConst x -> return (Value x)
  PreConjunct e1 e2 -> do
    e1' <- termFromPreterm e1
    e2' <- termFromPreterm e2
    return (Conjunct e1' e2')
  PreDisjunct e1 e2 -> do
    e1' <- termFromPreterm e1
    e2' <- termFromPreterm e2
    return (Disjunct e1' e2')
  PreComplement e -> do
    e' <- termFromPreterm e
    return (Complement e')
  PreAlways loc1 e1
    | PreEventually _ e2 <- e1 -> do
      guardLevel L3
      name <- fresh
      e2' <- local (const L2) (termFromPreterm e2)
      return (InfinitelyOften loc1 name e2')
    | otherwise -> do
      guardLevel L3
      name <- fresh
      e1' <- local (const L2) (termFromPreterm e1)
      return (Always loc1 name e1')
  PreEventually loc1 e1
    | PreAlways _ e2 <- e1 -> do
      guardLevel L3
      name <- fresh
      e2' <- local (const L2) (termFromPreterm e2)
      return (StaysAs loc1 name e2')
    | otherwise -> do
      guardLevel L3
      name <- fresh
      e1' <- local (const L2) (termFromPreterm e1)
      return (Eventually loc1 name e1')
  PreUpUntil loc1 e1 e2 -> do
    guardLevel L3
    name <- fresh
    e1' <- local (const L2) (termFromPreterm e1)
    e2' <- local (const L2) (termFromPreterm e2)
    return (UpUntil loc1 name e1' e2')

-- | If @m@ is the provided expression level, 'guardLevel' guards @m@ from being greater than the expression level in
-- a 'NameSupply' context. If @m@ is larger than expected, 'guardLevel' will emit a level-mismatch exception.
--
-- @since 0.1.0.0
guardLevel :: ExprLevel -> NameSupply ()
guardLevel level = do
  maxLevel <- ask
  when (maxLevel < level) (throwLevelMismatch maxLevel level)
{-# INLINE guardLevel #-}

-- | @'throwLevelMismatch' m n@ throws a level-mismatch exception with the maximum expected level @m@ and the unexpected
-- level @n@ such that @m < n@.
--
-- @since 0.1.0.0
throwLevelMismatch :: ExprLevel -> ExprLevel -> NameSupply a
throwLevelMismatch maxLevel level = throwError (LevelMismatch maxLevel level)
{-# INLINE throwLevelMismatch #-}
