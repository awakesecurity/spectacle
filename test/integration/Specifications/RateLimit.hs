{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

-- |
--
-- @since 0.1.0.0
module Specifications.RateLimit where

import Data.Hashable (Hashable)
import Language.Spectacle
  ( Action,
    ActionType (ActionUF),
    Fairness (Unfair),
    Specification (Specification),
    interaction,
    oneOf,
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

interact :: IO ()
interact = interaction rateLimitSpec

-- ---------------------------------------------------------------------------------------------------------------------

data Constants = Constants
  { constLifetime :: Int
  , constMaxCalls :: Int
  }

newtype Call = Call
  {getCallLifetime :: Int}
  deriving
    (Eq, Hashable, Num, Ord, Show)
    via Int

-- ---------------------------------------------------------------------------------------------------------------------

type RateLimitSpec = Specification SpecVar SpecActions SpecProp

type SpecVar =
  '[ "calls" # [Call]
   ]

type SpecActions =
  '[ "nextSend" # 'Unfair
   , "nextWait" # 'Unfair
   ]

type SpecProp =
  '[
   ]

rateLimitSpec :: RateLimitSpec
rateLimitSpec =
  let specInit = ConF #calls (pure []) NilF
      specNext =
        ConF #nextSend (ActionUF nextSend)
          . ConF #nextWait (ActionUF nextWait)
          $ NilF
      specProp = NilF
   in Specification {..}
  where
    ?constants = rateLimitConstants

rateLimitConstants :: Constants
rateLimitConstants =
  let constLifetime = 2
      constMaxCalls = 5
   in Constants {..}

nextSend :: (?constants :: Constants) => Action SpecVar Bool
nextSend = do
  numCalls <- length <$> plain #calls
  newCalls <- makeCalls numCalls
  #calls .= fmap (mappend newCalls) (plain #calls)

  pure (numCalls < constMaxCalls)
  where
    Constants {..} = ?constants

    makeCalls :: Int -> Action SpecVar [Call]
    makeCalls numCalls = do
      count <- oneOf [1 .. constMaxCalls - numCalls]
      pure (replicate count 0)

nextWait :: (?constants :: Constants) => Action SpecVar Bool
nextWait = do
  calls <- rmLimitCalls <$> plain #calls
  #calls .= pure (map (1 +) calls)

  pure True
  where
    Constants {..} = ?constants

    rmLimitCalls :: [Call] -> [Call]
    rmLimitCalls = filter \msg ->
      getCallLifetime msg < constLifetime
