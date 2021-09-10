-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.MCErrorList
  ( -- *
    MCErrorList (MCErrorList),
    pattern MCErrorLefts,
    pattern MCErrorRights,
    getErrorList,
  )
where

import Data.Kind (Type)

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'MCErrorList' is a left-biased monoid wrapping 'Either', Left annihilates Right.
--
-- @since 0.1.0.0
newtype MCErrorList :: Type -> Type -> Type where
  MCErrorList :: {getErrorList :: Either a b} -> MCErrorList a b
  deriving ()

pattern MCErrorLefts :: a -> MCErrorList a b
pattern MCErrorLefts xs <-
  MCErrorList (Left xs)
  where
    MCErrorLefts xs = MCErrorList (Left xs)

pattern MCErrorRights :: b -> MCErrorList a b
pattern MCErrorRights xs <-
  MCErrorList (Right xs)
  where
    MCErrorRights xs = MCErrorList (Right xs)

{-# COMPLETE MCErrorLefts, MCErrorRights #-}

-- | @since 0.1.0.0
instance (Semigroup a, Semigroup b) => Semigroup (MCErrorList a b) where
  MCErrorRights xs <> MCErrorRights ys = MCErrorRights (xs <> ys)
  MCErrorLefts xs <> MCErrorRights _ = MCErrorLefts xs
  MCErrorRights _ <> MCErrorLefts ys = MCErrorLefts ys
  MCErrorLefts xs <> MCErrorLefts ys = MCErrorLefts (xs <> ys)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance (Semigroup a, Monoid b) => Monoid (MCErrorList a b) where
  mempty = MCErrorRights mempty
  {-# INLINE CONLIKE mempty #-}

-- | @since 0.1.0.0
