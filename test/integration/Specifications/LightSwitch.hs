{-# LANGUAGE OverloadedLabels #-}

-- | Two-state on\/off cycle with a separate @WeakFair@ action for each
-- direction. The state graph is the 2-state BSCC @{Off, On}@, and both
-- @flipOn@ and @flipOff@ are weak-fair, enabled at exactly one state of the
-- cycle each but taken on every edge that exits that state. The cycle is
-- therefore fair under WF on both actions.
--
-- The interesting bit for the SCC checker is that two distinct WF actions
-- jointly satisfy the cycle's fairness obligation. The pre-SCC tree-walking
-- @expandAction@ would @local@-delete only one of them per branch, leaving
-- terminal stutters at @On@ or @Off@ in deeper subtrees and refuting either
-- @isOnInfinitely@ or @isOffInfinitely@ depending on the visit order. The
-- SCC checker examines the BSCC directly: edges exist for @flipOn@ and
-- @flipOff@ so both are taken in the cycle, and @isFairScc@ returns 'True'.
module Specifications.LightSwitch where

import Language.Spectacle
  ( Action,
    ActionType (ActionWF),
    Fairness (WeakFair),
    Modality (Infinitely),
    Specification (Specification),
    Temporal,
    TemporalType (PropGF),
    interaction,
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

interactLightSwitchSpec :: IO ()
interactLightSwitchSpec = interaction lightSwitchSpec

-- ---------------------------------------------------------------------------------------------------------------------

type LightVars = '["light" # Bool]

type LightSwitchSpec =
  Specification
    LightVars
    '[ "flipOn" # 'WeakFair
     , "flipOff" # 'WeakFair
     ]
    '[ "isOnInfinitely" # 'Infinitely
     , "isOffInfinitely" # 'Infinitely
     ]

flipOn :: Action LightVars Bool
flipOn = do
  light <- plain #light
  #light .= pure True
  pure (not light)

flipOff :: Action LightVars Bool
flipOff = do
  light <- plain #light
  #light .= pure False
  pure light

isOn :: Temporal LightVars Bool
isOn = plain #light

isOff :: Temporal LightVars Bool
isOff = do
  l <- plain #light
  pure (not l)

lightSwitchSpec :: LightSwitchSpec
lightSwitchSpec =
  Specification
    { specInit = ConF #light (pure False) NilF
    , specNext =
        ConF #flipOn (ActionWF flipOn)
          . ConF #flipOff (ActionWF flipOff)
          $ NilF
    , specProp =
        ConF #isOnInfinitely (PropGF isOn)
          . ConF #isOffInfinitely (PropGF isOff)
          $ NilF
    }
