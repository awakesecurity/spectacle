{-# LANGUAGE OverloadedLabels #-}

-- |
--
-- @since 0.1.0.0
module Specifications.Status where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import Language.Spectacle
  ( Action,
    ActionType (ActionSF, ActionUF, ActionWF),
    Fairness (StrongFair, Unfair, WeakFair),
    Modality (Eventually),
    Specification (Specification),
    Temporal,
    TemporalType (PropF),
    interaction,
    modelcheck,
    plain,
    specInit,
    specNext,
    specProp,
    (.=),
    pattern ConF,
    pattern NilF,
    type (#),
  )

-- ---------------------------------------------------------------------------------------------------------------------

statusSpecInteract :: IO ()
statusSpecInteract = do
  interaction statusSpec

statusSpecCheck :: IO ()
statusSpecCheck = do
  modelcheck statusSpec >>= \case
    Left err -> print err
    Right xs -> print xs

-- ---------------------------------------------------------------------------------------------------------------------

type StatusSpec =
  Specification
    '[ "status" # Status
     ]
    '[ "statusRetry" # 'StrongFair
     , "statusDone" # 'WeakFair
     , "statusFail" # 'Unfair
     ]
    '[ "isStatusDone" # 'Eventually
     ]

data Status = Start | Done | Fail
  deriving stock (Eq, Enum, Generic, Show)

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

isStatusDone :: Temporal '["status" # Status] Bool
isStatusDone = do
  status <- plain #status
  pure (status == Done)

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
        ConF #isStatusDone (PropF isStatusDone) NilF
    }
