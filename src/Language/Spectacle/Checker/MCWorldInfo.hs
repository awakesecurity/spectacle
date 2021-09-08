-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.MCWorldInfo
  ( -- *
    MCWorldInfo (MCWorldInfo),
    mcWorldInfoEnables,

    -- **
  )
where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- -------------------------------------------------------------------------------------------------------------------

data MCWorldInfo = MCWorldInfo
  { mcWorldInfoEnables :: Map String IntSet
  }
  deriving Show

-- | @since 0.1.0.0
instance Semigroup MCWorldInfo where
  MCWorldInfo e1 <> MCWorldInfo e2 = MCWorldInfo (Map.unionWith IntSet.union e1 e2)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid MCWorldInfo where
  mempty = MCWorldInfo Map.empty
  {-# INLINE CONLIKE mempty #-}
