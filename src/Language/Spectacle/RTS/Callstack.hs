-- | Spectacle call stacks.
--
-- @since 0.1.0.0
module Language.Spectacle.RTS.Callstack
  ( Callstack (..),
    pushCall,
    isCircular,
    prettyCallstack,
  )
where

import Control.Monad (join)
import Data.List (intersperse)

import Language.Spectacle.Syntax.Name (Name, reifyName)

-- -----------------------------------------------------------------------------

-- | A call stack of variable names accumulated during the execution of a
-- Spectacle program.
--
-- @since 0.1.0.0
newtype Callstack where
  Callstack :: [String] -> Callstack
  deriving (Eq, Show)

-- | @since 0.1.0.0
instance Semigroup Callstack where
  Callstack cs1 <> Callstack cs2 = Callstack (cs1 <> cs2)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid Callstack where
  mempty = Callstack mempty
  {-# INLINE mempty #-}

-- | Push the usage of a variable onto a 'Callstack'.
--
-- @since 0.1.0.0
pushCall :: Name nm -> Callstack -> Callstack
pushCall name (Callstack calls) = Callstack (reifyName name : calls)
{-# INLINE pushCall #-}

-- | Predicate testing if there exists a cycle in the usage of variables.
--
-- @since 0.1.0.0
isCircular :: Name nm -> Callstack -> Bool
isCircular name (Callstack calls) = reifyName name `elem` calls
{-# INLINE isCircular #-}

-- | Pretty print a 'Callstack'.
--
-- @since 0.1.0.0
prettyCallstack :: Callstack -> String
prettyCallstack (Callstack calls) = join (intersperse " -> " calls)
{-# INLINE prettyCallstack #-}
