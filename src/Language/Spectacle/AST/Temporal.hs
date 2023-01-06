-- |
-- Module      :  Language.Spectacle.AST.Temporal
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
-- 
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.AST.Temporal
  ( -- * Temporal Formula Monad
    Temporal,
    runTemporal,

    -- ** Effect Signature
    TemporalSyntax,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import Data.Type.Rec (Ascribe, Rec)
import Language.Spectacle.Lang (EffectK, Lang, runLang)
import Language.Spectacle.Syntax.Plain (Plain, runPlain)
import Language.Spectacle.Syntax.Prime (Prime, substPrime)

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
