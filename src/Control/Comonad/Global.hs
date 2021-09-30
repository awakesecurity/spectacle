{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
--
-- === Reference
--
-- 1. "Towards a Common Categorical Semantics Linear-Time Temporal Logic Functional Reactive Programming",
--   Wolfgang Jeltsch
--
-- @since 0.1.0.0
module Control.Comonad.Global where

import Control.Applicative (Alternative ((<|>)), liftA2)
import Control.Comonad (Comonad (extract))
import Control.Comonad.Trans.Cofree (CofreeF ((:<)), CofreeT (runCofreeT), coiterT)
import Control.Monad.Except (ExceptT, MonadError, throwError)
import Control.Monad.State.Strict (MonadState, MonadTrans (lift), StateT)
import Data.Kind (Type)
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Spectacle.Checker.MCError (MCError (MCAlwaysError))
import Lens.Micro.Mtl (use, view, (%=))

import Data.Context (Context)
import Data.Hashable (Hashable)
import Data.Temporal (ARel (ARel, relation, representitive), Interval (Interval), Time (Inf, Time), boundryElem)
import Data.Type.Rec (Rec)
import Data.World (World, worldFingerprint, worldValues)
import Language.Spectacle.AST (Action, runAction)
import Language.Spectacle.Model.Internal (ModelT (ModelT))
import Language.Spectacle.Model.MCState (MCState, mcStateCoverage, mcStateIsMarked)

-- --------------------------------------------------------------------------------------------------------------------

newtype GlobalT :: Context -> (Type -> Type) -> Type -> Type -> Type where
  GlobalT ::
    { runGlobalT ::
        CofreeT (StateT MCState (ExceptT [MCError ctxt] m)) (ARel (Set (World ctxt)) i) a
    } ->
    GlobalT ctxt m i a

globalNT ::
  (Monad m, Show i) =>
  GlobalT ctxt m i (Set (World ctxt)) ->
  ModelT ctxt m [Interval i (World ctxt) (Set (World ctxt))]
globalNT gl =
  let step = runCofreeT (runGlobalT gl)
      ix = relation step
   in case (representitive step, extract step) of
        (Inf, _) -> return [Interval ix Inf Inf]
        (Time t, to :< to')
          | to == t -> return (foldr ((:) . boundryElem ix) [] t)
          | otherwise -> do
            intervals <- foldr (liftA2 (++) . globalIntervals ix to) (pure []) t
            return intervals <|> (ModelT (lift to') >>= globalNT . GlobalT)

globalIntervals ::
  (MonadState MCState m, MonadError [MCError ctxt] m, Show i) =>
  i ->
  Set (World ctxt) ->
  World ctxt ->
  m [Interval i (World ctxt) (Set (World ctxt))]
globalIntervals ix there here
  | Set.null there = throwError [MCAlwaysError here (Set.singleton (show ix))]
  | otherwise = do
    isMarked <- use (mcStateIsMarked here)
    if isMarked
      then return []
      else do
        mcStateCoverage %= Set.insert (view worldFingerprint here)
        return [Interval ix (Time here) (Time there)]

globally :: (Monad f, Hashable (Rec ctxt)) => i -> World ctxt -> Action ctxt Bool -> GlobalT ctxt f i (Set (World ctxt))
globally i world act =
  let initialComonad = ARel i (globalStep act) (Time (Set.singleton world))
      coalgebra = pure . globalCoalgebra
   in GlobalT (coiterT coalgebra initialComonad)

globalStep :: Hashable (Rec ctxt) => Action ctxt Bool -> Time (Set (World ctxt)) -> Set (World ctxt)
globalStep _ Inf = Set.empty
globalStep act (Time ws) = foldMap ((`runAction` act) . view worldValues) ws

globalCoalgebra :: ARel (Set (World ctxt)) i (Set (World ctxt)) -> ARel (Set (World ctxt)) i (Set (World ctxt))
globalCoalgebra (ARel ix f x) = case x of
  Inf -> ARel ix (const Set.empty) Inf
  Time ws -> ARel ix f (Time (f (Time ws)))
