{-# LANGUAGE UndecidableInstances #-}

module Language.Spectacle.Spec.Coverage
  ( -- * Coverage Maps
    CoverageMap,
    HasCoverageMap (coverageMap),

    -- * Coverage Info
    CoverageInfo (CoverageInfo),
    emptyCoverageInfo,
    validSubformula,
    subsequentWorlds,
    checksCompleted,
    subformula,
    isValidSubformula,
  )
where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.TypeLits (Symbol)
import Lens.Micro (Lens', lens, (%~), (&), (^.))

import Data.Type.Rec (Ascribe, Rec)

-- ---------------------------------------------------------------------------------------------------------------------

type CoverageMap ctx = HashMap (Rec ctx) (CoverageInfo ctx)

type HasCoverageMap :: ([Ascribe Symbol Type] -> Type) -> Constraint
class HasCoverageMap f where
  coverageMap :: Lens' (f ctx) (CoverageMap ctx)

-- ---------------------------------------------------------------------------------------------------------------------

data CoverageInfo ctx = CoverageInfo
  { _validSubformula :: HashMap Int Bool
  , _subsequentWorlds :: Set (Rec ctx)
  , _checksCompleted :: Bool
  }

deriving instance Show (Rec ctx) => Show (CoverageInfo ctx)

emptyCoverageInfo :: CoverageInfo ctx
emptyCoverageInfo = CoverageInfo mempty Set.empty False
{-# INLINE CONLIKE emptyCoverageInfo #-}

validSubformula :: Lens' (CoverageInfo ctx) (HashMap Int Bool)
validSubformula = lens _validSubformula \info x -> info {_validSubformula = x}
{-# INLINE validSubformula #-}

subsequentWorlds :: Lens' (CoverageInfo ctx) (Set (Rec ctx))
subsequentWorlds = lens _subsequentWorlds \info x -> info {_subsequentWorlds = x}
{-# INLINE subsequentWorlds #-}

checksCompleted :: Lens' (CoverageInfo ctx) Bool
checksCompleted = lens _checksCompleted \info x -> info {_checksCompleted = x}
{-# INLINE checksCompleted #-}

subformula :: Int -> Lens' (CoverageInfo ctx) Bool
subformula name =
  lens
    (fromMaybe False . HashMap.lookup name . _validSubformula)
    \info x -> info & validSubformula %~ HashMap.insert name x
{-# INLINE subformula #-}

isValidSubformula :: Int -> CoverageInfo ctx -> Bool
isValidSubformula name info = HashMap.lookupDefault False name (info ^. validSubformula)
{-# INLINE isValidSubformula #-}
