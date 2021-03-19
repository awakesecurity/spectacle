{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 'Sum' is an extensible sum type indexed by a finite type universe. All
-- 'Sum' operations are constant time.
--
-- The implementation for sum was taken from Oleg Kiselyov's "Extensible
-- Effects: An Alternative to Monad Transformers" and improved upon by fixing
-- the semantics of the weakening rule so that they satisfy the coherence
-- condition for safe 'project'ion/'inject'ion.
--
-- Reference: http://okmij.org/ftp/Haskell/extensible/#open-union
--
-- @since 0.1.0.0
module Language.Spectacle.Lang.Sum
  ( -- * Extensible Sum
    Sum,
    type Syntax,
    type (|>) (..),
    decompose,
    extract,
    weaken,

    -- * Unsafe
    unsafeInject,
    unsafeProject,
  )
where

import Data.Kind (Constraint, Type)
import Unsafe.Coerce (unsafeCoerce)

import Data.Type.Induction (WInduct (reifyIx), WInductMaybe (reifyMaybeIx))

-- -----------------------------------------------------------------------------

-- | The kind of effects or spectacle syntax.
--
-- @since 0.1.0.0
type Syntax = (Type -> Type) -> Type -> Type

-- | 'Sum' is open union of effects indexed by a label set of effect kinds.
--
-- * @sig@ ~ A set of labels corresponding to effects that can be stored in a
-- 'Sum' type.
--
-- * @a@ ~ The type of the result of the stored effect.
--
-- @since 0.1.0.0
type Sum :: [Syntax] -> Type -> Type
data Sum sig a where
  Sum :: {-# UNPACK #-} !Word -> m a -> Sum sig a

-- | The purpose of the '(|>)' class is twofold:
--
-- * To constrain the 'Sum' signature such that for some @x '|>' sig@, the effect
-- @m@ occurs in @sig@. This allows for the safe injection of @m@ into a 'Sum'.
--
-- * To constrain the 'Sum' signature such that an @m@ can safely be projected
-- from a 'Sum' if it inhabits the sum.
--
-- [Coherence Condition]
--
-- @
-- 'project' . 'inject' == 'Just'
-- @
--
-- @since 0.1.0.0
infix 3 |>

type (|>) :: Syntax -> [Syntax] -> Constraint
class WInduct m sig => m |> sig where
  -- | Inject an effect @m t a@ into a 'Sum'.
  --
  -- @since 0.1.0.0
  inject :: m t a -> Sum sig a

  -- | Project a requested value of effect @m t a@ from the provided 'Sum', if
  -- and only if it inhabits the 'Sum'.
  --
  -- @since 0.1.0.0
  project :: Sum sig a -> Maybe (m t a)

-- | @since 0.1.0.0
instance WInduct @Syntax @[Syntax] m sig => m |> sig where
  inject = Sum (reifyIx @_ @_ @m @sig)
  {-# INLINE CONLIKE inject #-}

  project = unsafeProject (reifyIx @_ @_ @m @sig)
  {-# INLINE CONLIKE project #-}

-- | "Orthogonal projection" or decomposition of a 'Sum'. Returns 'Right' if the
-- requested effect @m t a@ inhabits the provided sum. Otherwise, the sum is
-- given back with the requested effect @m t a@ eliminated from its signature
-- wrapped in 'Left'.
--
-- @since 0.1.0.0
decompose :: Sum (m ': sig) a -> Either (Sum sig a) (m t a)
decompose (Sum 0 x) = Right (unsafeCoerce x)
decompose (Sum n x) = Left (Sum (n - 1) x)
{-# INLINE [2] decompose #-}

-- | 'extract' is a special case of 'project' that always works for sums whose
-- label set is singleton.
--
-- @since 0.1.0.0
extract :: Sum '[m] a -> m t a
extract (Sum _ a) = unsafeCoerce a
{-# INLINE extract #-}

-- | Weakening rule for 'Sum'. This says that adding effects to the signature of
-- the sum does not affect the effect inhabiting the sum.
--
-- @since 0.1.0.0
weaken ::
  forall m a sig.
  WInductMaybe m sig =>
  Sum sig a ->
  Sum (m ': sig) a
weaken (Sum n x) = case reifyMaybeIx @_ @_ @m @sig of
  -- The original implementation on Oleg's site implemented weakening such that
  -- if the sum provided was weakened with a @m@ which already occured in @sig@
  -- it opened up the possibility for
  --
  -- @project . weaken . inject = Nothing@
  --
  -- Which goes against the definition of a weakening rule and breaks type
  -- safety. 'reifyMaybeIx' solves this issue by searching the signature for a
  -- duplicate type and copies the index if a match is found. This makes weaken
  -- idempotent as it should be.
  Just ix -> Sum ix x
  Nothing -> Sum (n + 1) x
{-# INLINE weaken #-}

-- -----------------------------------------------------------------------------

-- | Lifts a type into a 'Sum' without any checks. Note that this can be only
-- used safely if you are __sure__ that type in the resulting 'Sum''s signature
-- at the index provided matches the value given, diagrammatically:
--
-- @
--           ╭╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╮
--           ╎        ╭────────────────────────────┤
--           ╎        │                            │
--           ╎        │                            │
--           ╎        │             1    2   ...   n
-- unsafeInj n (x :: m a) :: Sum '[f a, g a, ..., m a]
-- @
--
-- 'inject' should be used in place of 'unsafeInject'.
--
-- @since 0.1.0.0
unsafeInject :: Word -> m a -> Sum sig a
unsafeInject = Sum
{-# INLINE CONLIKE unsafeInject #-}

-- | Projects the type @m a@ from the 'Sum' provided resulting in 'Just' @m a@
-- if @m a@ inhabits the 'Sum', otherwise 'Nothing'.
--
-- 'project' should be used in place of 'unsafeProject'.
--
-- @since 0.1.0.0
unsafeProject :: Word -> Sum sig a -> Maybe (m a)
unsafeProject ix (Sum ix' x)
  | ix == ix' = Just (unsafeCoerce x)
  | otherwise = Nothing
{-# INLINE unsafeProject #-}
