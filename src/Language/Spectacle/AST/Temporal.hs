-- |
--
-- @since 0.1.0.0
module Language.Spectacle.AST.Temporal
  ( -- * Temporal Formula Monad
    Temporal,
    runTemporal,

    -- ** Effect Signature
    TemporalSyntax,
  )
where

import Data.Kind
import GHC.TypeLits
import Data.Function

import Data.Type.Rec
import Language.Spectacle.Lang
import Language.Spectacle.Syntax.Plain
import Language.Spectacle.Syntax.Prime

-- ---------------------------------------------------------------------------------------------------------------------

type Temporal :: [Ascribe Symbol Type] -> Type -> Type
type Temporal ctx = Lang ctx TemporalSyntax

type TemporalSyntax :: [EffectK]
type TemporalSyntax = '[Prime, Plain]

runTemporal :: Rec ctx -> Rec ctx -> Temporal ctx Bool -> Bool
runTemporal unprimed primed temporal =
  temporal
    & substPrime primed
    & runPlain unprimed
    & runLang
