module Language.Spectacle.Lang
  ( -- * CEK Machine
    Lang (..),
    runLang,
    evalLang,

    -- ** Syntax
    Syntax,
    type (|>),
    send,

    -- ** Interpreters
    interpret,
    interpretWith,
    reinterpretWith,
    interpose,
    interposeWith,
    queueApp,
    queueCompose,
    handleRelay,
    handleRelayS,
    replaceRelay,
    replaceRelayS,
    interposeRelay,
  )
where

import Control.Natural (type (~>))

import Data.Type.Induction (WInductMaybe)
import Language.Spectacle.Lang.Control (Control (Done, Next), Store (carried))
import Language.Spectacle.Lang.Internal
  ( Lang (Lang),
    queueApp,
    queueCompose,
    send,
  )
import Language.Spectacle.Lang.Interpreters
  ( handleRelay,
    handleRelayS,
    interposeRelay,
    replaceRelay,
    replaceRelayS,
  )
import Language.Spectacle.Lang.Sum (Syntax, type (|>))
import Language.Spectacle.Type.Rec (Rec)

-- -----------------------------------------------------------------------------

-- | Run a 'Lang' instance whose effects have been completely dispatched with
-- the provided store.
--
-- @since 0.1.0.0
runLang :: Rec cxt -> Lang '[] cxt a -> Store cxt a
runLang store (Lang m) = case m store of
  Done store' -> store'
  -- @Lang '[] cxt a@ cannot be constructed, only obtain by completely
  -- handling all effects in 'Lang' which always results in 'Done' so this
  -- this 'Next' case should be impossible to reach so long as 'Lang' is
  -- handled safely.
  Next _ -> error "runLang: pattern match on 'Next' should be impossible"
{-# INLINE runLang #-}

-- | Like 'runLang', 'evalLang' runs a 'Lang' instance whose have been completely
-- dispatched but returns the carried value while discarding the 'Store'.
--
-- @since 0.1.0.0
evalLang :: Rec cxt -> Lang '[] cxt a -> a
evalLang store = carried . runLang store
{-# INLINE evalLang #-}

-- | Interprets an effect given a natural transformation from the effect being
-- interpreted to 'Lang'.
--
-- @since 0.1.0.0
interpret ::
  (m (Lang sig cxt) ~> Lang sig cxt) ->
  Lang (m ': sig) cxt a ->
  Lang sig cxt a
interpret f = interpretWith \_ m -> (f m >>=)
{-# INLINE interpret #-}

-- | Interpret an effect given its continuation.
--
-- @since 0.1.0.0
interpretWith ::
  forall m a sig cxt.
  (forall x. Rec cxt -> m (Lang sig cxt) x -> (x -> Lang sig cxt a) -> Lang sig cxt a) ->
  Lang (m ': sig) cxt a ->
  Lang sig cxt a
interpretWith = handleRelay pure
{-# INLINE interpretWith #-}

-- | Reinterpret an effect @f@ as another effect @g@ given its continuation.
--
-- @since 0.1.0.0
reinterpretWith ::
  WInductMaybe g sig =>
  (forall x. Rec cxt -> f (Lang sig cxt) x -> (x -> Lang (g ': sig) cxt a) -> Lang (g ': sig) cxt a) ->
  Lang (f ': sig) cxt a ->
  Lang (g ': sig) cxt a
reinterpretWith = replaceRelay pure
{-# INLINE reinterpretWith #-}

-- | Interpose indermediate computations between effects.
--
-- @since 0.1.0.0
interpose ::
  m |> sig =>
  (m (Lang sig cxt) ~> Lang sig cxt) ->
  Lang sig cxt a ->
  Lang sig cxt a
interpose f = interposeWith \_ m -> (f m >>=)
{-# INLINE interpose #-}

-- | Interpose intermediate computations between effects given the effects
-- continuation.
--
-- @since 0.1.0.0
interposeWith ::
  m |> sig =>
  (forall x. Rec cxt -> m (Lang sig cxt) x -> (x -> Lang sig cxt a) -> Lang sig cxt a) ->
  Lang sig cxt a ->
  Lang sig cxt a
interposeWith = interposeRelay pure
{-# INLINE interposeWith #-}
