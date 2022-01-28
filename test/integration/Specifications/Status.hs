{-# LANGUAGE OverloadedLabels #-}

-- |
--
-- @since 0.1.0.0
module Specifications.Status where

import Data.Hashable
import GHC.Generics

import Data.Type.Rec
import Language.Spectacle.AST.Action
import Language.Spectacle.AST.Temporal
import Language.Spectacle.Model
import Language.Spectacle.Specification
import Language.Spectacle.Fairness
import Language.Spectacle.Syntax

-- ---------------------------------------------------------------------------------------------------------------------

type StatusSpec =
  Specification
    '[ "status" # Status
     ]
    '[ "statusRetry" # 'StrongFair
     , "statusDone" # 'WeakFair
     , "statusFail" # 'Unfair
     ]
    '[
     ]

data Status = Start | Done | Fail
  deriving (Eq, Enum, Generic, Show)

instance Hashable Status

statusRetry :: Action '["status" # Status] Bool
statusRetry = do
  status <- plain #status
  #status .= pure Start
  pure (status == Fail)

statusDone :: Action '["status" # Status] Bool
statusDone = do
  status <- plain #status
  #status .= pure Done
  pure (status == Start)

statusFail :: Action '["status" # Status] Bool
statusFail = do
  status <- plain #status
  #status .= pure Fail
  pure (status == Start)

statusSpec :: StatusSpec
statusSpec =
  Specification
    { specInit =
        ConF #status (pure Start) NilF
    , specNext =
        ConF #statusRetry (ActionSF statusRetry)
          . ConF #statusDone (ActionWF statusDone)
          . ConF #statusFail (ActionUF statusFail)
          $ NilF
    , specProp =
        NilF
    }

statusSpecCheck :: IO ()
statusSpecCheck = do
  modelcheck statusSpec >>= \case
    Left err -> print err
    Right xs -> print xs
