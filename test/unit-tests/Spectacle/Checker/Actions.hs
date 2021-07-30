{-# LANGUAGE OverloadedLabels #-}

module Spectacle.Checker.Actions
  ( IncSpec,
    actionInc,
    OrSpec,
    actionOr,
  )
where

import Numeric.Natural

import Data.Type.Rec
import Language.Spectacle.AST.Action
import Language.Spectacle.Syntax

-- ---------------------------------------------------------------------------------------------------------------------

type IncSpec = '["x" # Natural]

actionInc :: Natural -> Action IncSpec Bool
actionInc n = do
  x <- plain #x
  #x .= return (min (x + n) 24)
  return True

type OrSpec = '["x" # Natural]

actionOr :: Action OrSpec Bool
actionOr = (#x .= oneOf [] >> return True) \/ (#x .= return 5 >> return True)
