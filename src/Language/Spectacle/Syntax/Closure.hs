-- | Definitions and interpreters for the 'Closure' syntax.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Closure
  ( -- * Closures
    Closure (..),
    (.=),

    -- ** Running Closures
    runClosures,
    registerClosures,
    runSubst,
    callEnv,

    -- * Expressions
    type Expr,
    normExpr,
    quoteExpr,
    evalExpr,
  )
where

import Control.Monad (when)
import Data.Function ((&))

import Language.Spectacle.Effect.NonDet
  ( NonDet,
    oneOf,
    runNonDetAll,
  )
import Language.Spectacle.Lang
  ( Lang,
    Syntax,
    evalLang,
    handleRelayS,
    send,
    type (|>),
  )
import Language.Spectacle.RTS.Callstack
  ( Callstack,
    isCircular,
    pushCall,
  )
import Language.Spectacle.RTS.Env
  ( Env (Env, bindings),
    Environment,
    Value (Neutral, Value),
    environment,
    getRegisters,
    locally,
    runEnvironment,
    setEnvNeutral,
    setRegisterValue,
  )
import Language.Spectacle.RTS.Exception
  ( RuntimeError,
    RuntimeException,
    runError,
    throwCyclicReference,
    throwError,
  )
import Language.Spectacle.Syntax.Quantifier (Quantifier, runQuantifier)
import Language.Spectacle.Syntax.Subst (Subst (..))
import Language.Spectacle.Type.Rec
  ( Name,
    Rec,
    getRec,
    type (#),
    type (<:),
  )

-- -----------------------------------------------------------------------------

-- | The syntax for closures.
--
-- * @'Name' nm@ is the name of a variable of type @a@ occuring in the context
-- @cxt@ whose value is the result of evaluating this closure.
--
-- * @'Expr' cxt a@ is the body of the closure resulting in the modified
-- environment @'Env' cxt@ where neutral term for this closure is replaced by
-- the set of values obtained by evaluting this expression.
--
-- @since 0.1.0.0
data Closure :: Syntax where
  Closure ::
    (nm # a <: cxt, m ~ Lang sig cxt) =>
    Name nm ->
    Expr cxt a ->
    Closure m (Env cxt)

-- | The syntax for defining relations of variables in a Spectacle program.
-- This can be read as "The new value of the variable @nm@ is the result of
-- evaluating @Expr cxt a@" or "The variable @nm@ is related to @Env cxt@
-- by @Expr cxt a@".
--
-- A simple example of the '(.=)' syntax would be a natural typed variable
-- named @counter@ which discretely increments with respect to time:
--
-- @
-- #counter .= do
--   -- The value of #counter in the previous frame of time:
--   counter :: Natural <- plain #counter
--   return (counter + 1)
-- @
--
-- @since 0.1.0.0
(.=) ::
  (nm # a <: cxt, Closure |> sig) =>
  Name nm ->
  Expr cxt a ->
  Lang sig cxt (Env cxt)
name .= fn = send (Closure name fn)
{-# INLINE (.=) #-}

-- -----------------------------------------------------------------------------

-- | Evaluates the expressions in the bodies of closures in the current
-- environment using a call-by-need strategy.
--
-- @since 0.1.0.0
runClosures ::
  (Environment |> sig, NonDet |> sig, RuntimeError |> sig) =>
  Lang (Closure ': sig) cxt a ->
  Lang sig cxt (Env cxt)
runClosures m = do
  initialEnv <- registerClosures m
  m & handleRelayS initialEnv (const . pure) \store env cl k -> case cl of
    -- The type annotation here is necessary to unify the result of evaluating
    -- the closure body with the @nm # x <: fn@ given by the constructor for
    -- 'Closure'. It might be better to have 'registerClosures' reinterpret
    -- into a "call" effect which discards the body and clean this up in the
    -- future.
    --
    -- Another source of trickiness here is that the @nm@ and @cxt@ parameters
    -- do not uniquely determine @x@ for the current implementation (3/19/2021)
    -- of 'RecT'. It would be better to reformulate 'RecT' so that its obvious
    -- that @nm@ can only mean @x@ to the typechecker, if possible.
    Closure name (_ :: Expr sig x) -> do
      (env', _ :: x) <- callEnv store env name
      locally env' (environment >>= k env')

-- | Constructs the initial environment for interpreting a Spectacle relation
-- given the initial state (or store) for the relation.
--
-- @since 0.1.0.0
registerClosures ::
  (Environment |> sig) =>
  Lang (Closure ': sig) cxt a ->
  Lang sig cxt (Env cxt)
registerClosures m = do
  initEnv <- environment
  m & handleRelayS initEnv (const . pure) \_ env (Closure name fn) k ->
    let env' = setEnvNeutral name fn env
     in locally env (environment >>= k env')

-- | Substitutes the usage of plain variables with values in a 'Lang' store and
-- primed variables with values in the 'Lang' environment.
--
-- @since 0.1.0.0
runSubst ::
  (Environment |> sig, NonDet |> sig, RuntimeError |> sig) =>
  Lang (Subst ': sig) cxt a ->
  Lang sig cxt (Env cxt, a)
runSubst m = do
  -- Preferably, this would be defined in the same place as 'Subst', but the
  -- close interactions between env/subst/closure means this cannot be done
  -- without introducing import cycles as of 3/19/2021.
  initEnv <- environment
  m & handleRelayS initEnv (curry pure) \store env substs k ->
    case substs of
      Plain name -> k env (getRec store name)
      Prime name -> do
        (env', x) <- callEnv store env name
        locally env' (environment >>= (`k` x))
{-# INLINE runSubst #-}

-- | References the value of a primed name in the enclosing 'Env'. Under the
-- hood, this could either return a known value for the value or perform
-- call-by-need evaluation on a closure associated with the given name.
--
-- @since 0.1.0.0
callEnv ::
  (NonDet |> sig, RuntimeError |> sig, nm # a <: cxt) =>
  Rec cxt ->
  Env cxt ->
  Name nm ->
  Lang sig cxt (Env cxt, a)
callEnv store env name = case getRegisters name (bindings env) of
  Value _ x -> return (env, x)
  Neutral _ fn -> evalExpr store env name fn
{-# INLINE callEnv #-}

-- -----------------------------------------------------------------------------

-- | A type synonym for Spectacle expressions. 'Lang' is indexed by the
-- syntax/effects that can be used in expressions.
--
-- @since 0.1.0.0
type Expr =
  Lang
    '[ Quantifier
     , Subst
     , Environment
     , NonDet
     , RuntimeError
     ]

-- | Normalizes an 'Expr' producing either a runtime exception or a list of
-- possible values the expression could be evaluated in its current environment,
-- i.e.
--
-- @
-- -- the result of evaluating this expression could take on either 2 or 4
-- normExpr store env (exists [1..5] even) == Right [2,4]
--
-- -- results are just singletons for simple expressions
-- normExpr store env (return 1) == Right [1]
-- @
--
-- @since 0.1.0.0
normExpr ::
  Rec cxt -> -- the lang store
  Env cxt -> -- the environment for the expr
  Expr cxt a -> -- the expr to normalize
  Either RuntimeException [(Env cxt, a)]
normExpr store env expr =
  expr
    -- It may not be necessary to fully evaluate 'Expr' all the way down to the
    -- encapsulating 'Lang', instead it might be easier just to discharge effects
    -- up to the greatest common prototype shared between spectacle
    -- syntax/effects.
    & runQuantifier
    & runSubst
    & runEnvironment env
    & runNonDetAll @[]
    & runError
    & evalLang store

-- | "Quotes" the result of evaluating an 'Expr' yielding a environment/value
-- pair within the list of possible values in the provided result list chosen
-- nondeterministically. The inverse of 'normExpr'.
--
-- @since 0.1.0.0
quoteExpr ::
  (NonDet |> sig, RuntimeError |> sig, nm # a <: cxt) =>
  Callstack ->
  Name nm ->
  Either RuntimeException [(Env cxt, a)] -> -- expr result to quote
  Lang sig cxt (Env cxt, a)
quoteExpr stack name = \case
  -- "Quotes" the result of evaluating an expression (typically with
  -- 'normExpr'). The callstack installed into the environment for the
  -- quoted expression can be arbitrary, but is especially useful to
  -- dequeue names pushed onto the stack by the interpreter.
  Right envs' -> do
    (Env binds' _, x) <- oneOf envs'
    return (Env (setRegisterValue name x binds') stack, x)
  Left err -> throwError err

-- | Evaluates an expression given a store, environment, and the name of the closure
-- that the expression originated from. While 'quoteExpr' and 'normExpr' cannot be
-- composed directly, 'evalExpr' should be roughly thought of as:
--
-- @
-- quoteExpr . normExpr == evalExpr
-- @
--
-- @since 0.1.0.0
evalExpr ::
  (NonDet |> sig, RuntimeError |> sig, nm # a <: cxt) =>
  Rec cxt ->
  Env cxt ->
  Name nm ->
  Expr cxt a ->
  Lang sig cxt (Env cxt, a)
evalExpr store (Env binds stack) name expr = do
  -- Here we check the callstack of the provided environment to see if the
  -- variable identified by @name@ has already been evaluated within the current
  -- environment and throw a cyclic reference exception if evaluating a pair of
  -- closures depend eachother.
  when (isCircular name stack) (throwCyclicReference stack)
  let result = normExpr store (Env binds (pushCall name stack)) expr
  quoteExpr stack name result
