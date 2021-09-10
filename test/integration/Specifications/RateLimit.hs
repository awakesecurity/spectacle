{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

-- |
--
-- @since 0.1.0.0
module Specifications.RateLimit where

import Data.Functor
import Data.Hashable

import Language.Spectacle (Action, (.=), plain)
import Language.Spectacle.Interaction (defaultInteraction)
import Language.Spectacle.Specification
  ( Always,
    Eventually,
    Fairness (Unfair, WeakFair),
    Spec(Spec),
    Var((:=)),
    type VariableCtxt,
    type (!>)(UnfairAction, WeakFairAction),
    type (/\),
    type (\/)((:\/:)),
  )

-- ---------------------------------------------------------------------------------------------------------------------

-- Constants we will use throughout the specification
data RateLimitConsts = RateLimitConsts
  { window :: Int -- The interval of time we want to limit messages in.
  , msgLimit :: Int -- The maximum number of messages we can receive with the set interval.
  }

newtype Message = Message {msgAge :: Int}
  deriving (Hashable, Show)
  deriving Num via Int

-- ---------------------------------------------------------------------------------------------------------------------

type RateLimitSpec =
  Spec
    (Var "msgLog" [Message])
    ("sendMessage" !> 'Unfair \/ "passTime" !> 'Unfair)
    (Always "passTime" /\ Eventually "sendMessage")

sendMessage :: (?constants :: RateLimitConsts) => Action (VariableCtxt RateLimitSpec) Bool
sendMessage = do
  let RateLimitConsts {..} = ?constants
  msgLog <- plain #msgLog

  #msgLog .= return (0 : msgLog)

  return (length msgLog <= msgLimit)

passTime :: (?constants :: RateLimitConsts) => Action (VariableCtxt RateLimitSpec) Bool
passTime = do
  let RateLimitConsts {..} = ?constants

  #msgLog .= (filter ((<= window) . msgAge) . map (1 +) <$> plain #msgLog)

  return True

spec :: RateLimitSpec
spec = Spec specInit specNext
  where
    specInit = #msgLog := return []

    specNext =
      let ?constants = RateLimitConsts { window = 2 , msgLimit = 5 }
      in UnfairAction #sendMessage sendMessage
           :\/: UnfairAction #passTime passTime

check :: IO ()
check = defaultInteraction spec
