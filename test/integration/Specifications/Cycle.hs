-- |
--
-- @since 0.1.0.0
module Specifications.Cycle
  ( Cycle (Cycle, runCycle)
  )
where

-- ---------------------------------------------------------------------------------------------------------------------

newtype Cycle :: Type -> Type where
  Cycle ::
    { runCycle ::
        LevelsT (StateT (IntMap (Set Fingerprint)) (WriterT [Fingerprint] [])) a
    } -> Cycle a

takeCyclesFrom :: IntMap (Set Fingerprint) -> Fingerprint -> _
takeCyclesFrom graph goal =
  stepCycleInitial graph goal
    & _

stepCycleInitial ::
  IntMap (Set Fingerprint) ->
  Fingerprint ->
  Cycle Fingerprint
stepCycleInitial graph goal =

stepCycleInitial ::
  IntMap (Set Fingerprint) ->
  Fingerprint ->
  Fingerprint ->
  Cycle Fingerprint
stepCycleInitial graph goal here =
