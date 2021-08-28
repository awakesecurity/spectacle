-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.Graph
  ( EquivalenceClass (EquivalenceClass, ReflexiveClass),
    actionQuotient,
    pruneUnrelated,
  )
where

import Data.Hashable
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Type.Rec
import Language.Spectacle.AST
import Language.Spectacle.Checker.Model.MCError
import Language.Spectacle.Checker.World
import Language.Spectacle.RTS.Registers (RuntimeState (newValues))

-- ---------------------------------------------------------------------------------------------------------------------

data EquivalenceClass a
  = EquivalenceClass (Set a)
  | ReflexiveClass

actionQuotient ::
  Hashable (Rec ctx) =>
  World ctx ->
  Action ctx Bool ->
  Either (MCError ctx) (EquivalenceClass (World ctx))
actionQuotient worldHere@(World _ values) action =
  case pruneUnrelated <$> runAction values action of
    Left err -> Left (MCActionError worldHere err)
    Right worldsThere
      | Set.null worldsThere -> Left (MCImpasseError worldHere)
      | worldsThere == Set.singleton worldHere -> Right ReflexiveClass
      | otherwise -> do
          let worldsThere' = Set.delete worldHere worldsThere
          Right (EquivalenceClass worldsThere')

pruneUnrelated :: Hashable (Rec ctx) => [(RuntimeState ctx, Bool)] -> Set (World ctx)
pruneUnrelated [] = Set.empty
pruneUnrelated ((vs, rel) : xs)
  | rel = Set.insert (newWorld (newValues vs)) (pruneUnrelated xs)
  | otherwise = pruneUnrelated xs
