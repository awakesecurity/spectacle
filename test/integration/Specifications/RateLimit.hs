{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

-- |
--
-- @since 0.1.0.0
module Specifications.RateLimit
  ( check,
  )
where

import Data.Functor ((<&>))
import Data.Hashable (Hashable)

import Language.Spectacle
  ( Action,
    Initial,
    Invariant,
    always,
    defaultInteraction,
    define,
    modelCheck,
    plain,
    unfair,
    (.=),
    (\/),
    type (#),
  )
import Language.Spectacle.Specification
  ( Specification (Specification),
    fairnessConstraint,
    initialAction,
    nextAction,
    temporalFormula,
    terminationFormula,
  )

-- ---------------------------------------------------------------------------------------------------------------------

-- Constants we will use throughout the specification
data RateLimitConsts = RateLimitConsts
  { window :: Int -- The interval of time we want to limit messages in.
  , msgLimit :: Int -- The maximum number of messages we can receive with the set interval.
  }

-- The type of messages only considering their age since the message was received.
newtype Message = Message {msgAge :: Int}
  deriving (Hashable, Show)

-- A declaration of variables in the specification. Read as a variable "msgLog" with type @[Message]@.
type RateLimitSpec = '["msgLog" # [Message]]

specInit :: Initial RateLimitSpec ()
specInit = do
  -- The variable "msgLog" is defined as the empty list.
  #msgLog `define` return []

specNext :: (?constants :: RateLimitConsts) => Action RateLimitSpec Bool
specNext =
  -- Either perform a "sendMessage" or "passTime" action
  sendMessage \/ passTime

sendMessage ::
  -- -XImplicitParams for passing specification constants.
  (?constants :: RateLimitConsts) =>
  Action RateLimitSpec Bool
sendMessage = do
  -- -XRecordWildcard for bringing the constants "window" and "msgLimit" into scope.
  let RateLimitConsts {..} = ?constants
  msgLog <- plain #msgLog
  #msgLog .= return (Message 0 : msgLog)
  return (length msgLog < msgLimit + 1)

passTime ::
  (?constants :: RateLimitConsts) =>
  Action RateLimitSpec Bool
passTime = do
  let RateLimitConsts {..} = ?constants
  #msgLog
    .= ( -- Retrieve the current value of "msgLog".
         plain #msgLog
          -- Increment the age of each message.
          <&> map (\msg -> Message (msgAge msg + 1))
          -- Remove any message older than the limiting window from the message log.
          <&> filter (\msg -> msgAge msg <= window)
       )
  return True

specProp ::
  (?constants :: RateLimitConsts) =>
  Invariant RateLimitSpec Bool
specProp = do
  let RateLimitConsts {..} = ?constants
  always do
    msgLog <- plain #msgLog
    return (length msgLog <= msgLimit)

spec :: (?constants :: RateLimitConsts) => Specification RateLimitSpec
spec =
  Specification
    { initialAction = specInit
    , nextAction = specNext
    , temporalFormula = specProp
    , terminationFormula = Nothing
    , fairnessConstraint = unfair
    }

check :: IO ()
check = do
  let ?constants = RateLimitConsts {window = 10, msgLimit = 5}
  defaultInteraction (modelCheck spec)
