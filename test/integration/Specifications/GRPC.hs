{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- |

module Specifications.GRPC where

import Control.Monad
import Data.Type.Rec
import Data.Hashable
import Data.Traversable
import Data.List
import Data.Name
import Prelude
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Kind
import GHC.TypeLits
import GHC.Generics
import qualified Data.HashMap.Strict as HashMap
import Debug.Trace

import Data.Ascript
import Language.Spectacle.RTS.Registers (RelationTerm)
import Language.Spectacle
import Language.Spectacle.Specification

-- ---------------------------------------------------------------------------------------------------------------------

data Constants = Constants
  { numProcs    :: Int
  , maxChunks   :: Int
  , maxMessages :: Int
  }

type ProcessId = Int

data ProcessState = Running | Done

data Message = Message { msgId :: Int, chunks :: Int }
  deriving (Eq, Ord, Generic, Hashable, Show)

newtype Process = Process { channel :: Maybe Message }
 deriving (Generic, Hashable, Show)

-- ---------------------------------------------------------------------------------------------------------------------

type SpecGRPCMQTT :: [Ascribe Symbol Type]
type SpecGRPCMQTT =
  '[ "messages"  # [Message]
   , "processes" # HashMap Int Process
   ]

specInit :: (?constants :: Constants) => Initial SpecGRPCMQTT ()
specInit = do
  let Constants {..} = ?constants
  #messages  `define` return []
  #processes `define` return (HashMap.fromList [(n, Process Nothing) | n <- [0 .. numProcs]])

specNext :: (?constants :: Constants) => Action SpecGRPCMQTT Bool
specNext = do
  processes <- plain #processes
  messages  <- plain #messages
  trace (show processes) (pure ())
  trace (show messages) (pure ())
  newMessage \/ procNext

procNext :: (?constants :: Constants) => Action SpecGRPCMQTT Bool
procNext = do
  let Constants {..} = ?constants
  exists [0 .. numProcs] \procId -> do
    readChan procId
      \/ writeGRPC procId
      \/ consumeGRPC procId

newMessage :: (?constants :: Constants) => Action SpecGRPCMQTT Bool
newMessage = do
  let Constants {..} = ?constants
  messages <- plain #messages
  if length messages < maxMessages
    then do
      numChunks <- oneOf [0 .. maxChunks]
      -- TODO: Assign msg ID
      #messages .= return (Message 0 numChunks : messages)
      return True
    else return False

readChan :: (?constants :: Constants) => ProcessId -> Action SpecGRPCMQTT Bool
readChan procId = do
  messages <- plain #messages
  case messages of
    msg : msgs -> do
      process <- (HashMap.! procId) <$> plain #processes
      case channel process of
        Nothing  -> do
          #messages .= return msgs
          #processes .= #processes `except` (procId, Process (Just msg))
          return True
        Just msg -> return False
    [] -> return False

writeGRPC :: (?constants :: Constants) => ProcessId -> Action SpecGRPCMQTT Bool
writeGRPC procId = do
  process <- (HashMap.! procId) <$> plain #processes
  case channel process of
    Nothing  -> return False
    Just msg
      | chunks msg == 0 -> return False
      | otherwise -> do
          let msg' :: Message
              msg' = Message {msgId = msgId msg, chunks = chunks msg - 1 }
          #processes .= #processes `except` (procId, Process (Just msg'))
          return True

consumeGRPC :: ProcessId -> Action SpecGRPCMQTT Bool
consumeGRPC procId = do
  process <- (HashMap.! procId) <$> plain #processes
  case channel process of
    Nothing -> return False
    Just msg
      | chunks msg == 0 -> do
          #processes .= #processes `except` (procId, Process (Just msg))
          return True
      | otherwise -> return False

except :: (Eq a, Hashable a, s # HashMap a b .| ctx) => Name s -> (a, b) -> RelationTerm ctx (HashMap a b)
except name (k, v) = HashMap.insert k v <$> plain name

specProp :: (?constants :: Constants) => Invariant SpecGRPCMQTT Bool
specProp = do
  let Constants {..} = ?constants
  msgOrder /\ liveness
  where
    -- TODO: quotient msgId = msgId'
    doesDecrement :: Maybe Message -> Maybe Message -> Bool
    doesDecrement (Just (Message _ 0)) Nothing  = True
    doesDecrement Nothing  _ = True
    doesDecrement (Just x) (Just y)
      | chunks x /= chunks y = chunks x - 1 == chunks y
      | otherwise = True
    doesDescrement _ _ = False

    msgOrder :: Invariant SpecGRPCMQTT Bool
    msgOrder = always do
      let Constants {..} = ?constants
      and <$> forM [0 .. numProcs] \procId -> do
        process  <- channel . (HashMap.! procId) <$> plain #processes
        process' <- channel . (HashMap.! procId) <$> prime #processes
        return (doesDecrement process process')

    liveness :: Invariant SpecGRPCMQTT Bool
    liveness = eventually do
      let Constants {..} = ?constants
      messages  <- Set.fromList <$> plain #messages
      messages' <- Set.fromList <$> prime #messages
      case Set.toList (messages' `Set.difference` messages) of
        [msg] -> do
          processes <- plain #processes
          let p = flip any processes \(Process maybeMsg) -> case maybeMsg of
                Nothing -> False
                Just msg' -> do
                  trace (show msg') (msgId msg == msgId msg')
          return p
        _ -> return False


      -- -- [x, y, z]
      -- -- f x >> f y >> f z
      -- if null newMsg
      --   then return True
      --   else eventually do
      --     processes <- plain #processes
      --     and <$> forM (Set.toList newMsg) \msg ->
      --       or <$> forM processes \process -> case channel process of
      --         Nothing -> return False
      --         Just msg' -> return (msgId msg == msgId msg')




  -- new message in #messages ==> eventually the new messages is sent to a channel

  -- if we obtain a new message ==> eventually Channel msg == Nothing

  -- messages  <- plain #messsage
  -- messages' <- prime #messsage

  -- Props
  --
  -- Need to model the message order.
  --
  -- Chunks 3 --> Chunks 2 --> Chunks 1 --> Chunks 0 --> Done
  --
  -- forall ix :: Int -> (p : Process ix)
  -- if numChunks p /= 0
  --   then p' == Process Nothing
  --

-- TODO: always initial state is not checked
spec :: (?constants :: Constants) => Specification SpecGRPCMQTT
spec =
  Specification
    { initialAction = specInit
    , nextAction = specNext
    , temporalFormula = specProp
    , terminationFormula = Nothing
    , fairnessConstraint =weakFair
    }

check :: IO ()
check =
  let ?constants = Constants { numProcs = 2, maxMessages = 2, maxChunks = 2 }
  in defaultInteraction (modelCheck spec)
