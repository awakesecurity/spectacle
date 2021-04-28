{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Modal.Rewrite
  ( applyRewrites,
    normalize,
    rewriteConjunct,
  )
where

import Control.Applicative
import Control.Monad.Trans.State
import Data.Foldable
import Data.Kind
import GHC.TypeLits

import Data.Ascript
import Language.Spectacle.Lang
import Language.Spectacle.Syntax.Logic.Internal
import Language.Spectacle.Syntax.Modal.Internal
import Language.Spectacle.Syntax.Modal.Quoted
import Language.Spectacle.Syntax.NonDet.Internal

-- -------------------------------------------------------------------------------------------------

data Modality
  = ModalityAlways
  | ModalityUpUntil
  | ModalityEventually
  | ModalityNone

data RewriteContext = RewriteContext
  { rewriteModality :: Modality
  }

applyRewrites ::
  Members '[Modal, Logic, NonDet] effs =>
  ModalQ ctx effs Bool ->
  ModalQ ctx effs Bool
applyRewrites modal = evalState (normalize modal) (RewriteContext ModalityNone)

normalize ::
  Members '[Modal, Logic, NonDet] effs =>
  ModalQ ctx effs Bool ->
  State RewriteContext (ModalQ ctx effs Bool)
normalize = \case
  ConstQ x -> return (ConstQ x)
  ModalQ meta union k -> case meta of
    MetaAlways x -> rewriteAlways (WAlways x union k)
    MetaUpUntil lhs rhs -> rewriteUpUntil (WUpUntil lhs rhs union k)
    MetaForall xs -> rewriteForall (WForall xs union k)
    MetaExists xs -> rewriteExists (WExists xs union k)
    MetaComplement x -> rewriteComplement (WComplement x union k)
    MetaConjunct lhs rhs -> rewriteConjunct (WConjunct lhs rhs union k)
    MetaDisjunct lhs rhs -> rewriteDisjunct (WDisjunct lhs rhs union k)

setModality :: Modality -> State RewriteContext ()
setModality modality = modify \ctx -> ctx {rewriteModality = modality}

-- -------------------------------------------------------------------------------------------------

data Witnesses
  = WitAlways
  | WitUpUntil
  | WitEventually
  | WitForall
  | WitExists
  | WitComplement
  | WitConjunct
  | WitDisjunct

type HasWitness :: Witnesses -> Constraint
class HasWitness k where
  data Witness k :: [Ascribe Symbol Type] -> [EffectK] -> Type -> Type

  toWitness :: ModalQ ctx effs Bool -> Maybe (Witness k ctx effs Bool)

-- -------------------------------------------------------------------------------------------------

instance HasWitness 'WitAlways where
  data Witness 'WitAlways ctx effs a where
    WAlways ::
      ModalQ ctx effs b ->
      Union ctx effs a ->
      (a -> Lang ctx effs b) ->
      Witness 'WitAlways ctx effs b

  toWitness (ModalQ (MetaAlways m) u k) = Just (WAlways m u k)
  toWitness _ = Nothing

rewriteAlways ::
  Members '[Modal, Logic, NonDet] effs =>
  Witness 'WitAlways ctx effs Bool ->
  State RewriteContext (ModalQ ctx effs Bool)
rewriteAlways (WAlways modal union k)
  | Just (WAlways modal' union' k') <- toWitness modal = do
    setModality ModalityAlways
    rewriteAlways (WAlways modal' union' k')
  | Just (WUpUntil (ConstQ True) rhs union' k') <- toWitness modal
    , Just (WAlways _ _ _) <- toWitness rhs = do
    setModality ModalityUpUntil
    rewriteUpUntil (WUpUntil (ConstQ True) rhs union' k')
  | otherwise = do
    setModality ModalityAlways
    modal' <- normalize modal
    return (ModalQ (MetaAlways modal') union k)

-- -------------------------------------------------------------------------------------------------

instance HasWitness 'WitUpUntil where
  data Witness 'WitUpUntil ctx effs a where
    WUpUntil ::
      ModalQ ctx effs b ->
      ModalQ ctx effs b ->
      Union ctx effs a ->
      (a -> Lang ctx effs b) ->
      Witness 'WitUpUntil ctx effs b

  toWitness (ModalQ (MetaUpUntil lhs rhs) union k) = Just (WUpUntil lhs rhs union k)
  toWitness _ = Nothing

rewriteUpUntil ::
  Members '[Modal, Logic, NonDet] effs =>
  Witness 'WitUpUntil ctx effs Bool ->
  State RewriteContext (ModalQ ctx effs Bool)
rewriteUpUntil (WUpUntil lhs rhs union k)
  | ConstQ True <- lhs
    , ConstQ True <- rhs =
    return (ConstQ True)
  | ConstQ True <- lhs = do
    setModality ModalityEventually
    rewriteEventually (WEventually rhs union k)
  | otherwise = do
    setModality ModalityUpUntil
    liftA2 freshDisjunctQ (normalize lhs) (normalize rhs)

-- -------------------------------------------------------------------------------------------------

instance HasWitness 'WitEventually where
  data Witness 'WitEventually ctx effs a where
    WEventually ::
      ModalQ ctx effs b ->
      Union ctx effs a ->
      (a -> Lang ctx effs b) ->
      Witness 'WitEventually ctx effs b

  toWitness (ModalQ (MetaUpUntil (ConstQ True) modal) union k) = Just (WEventually modal union k)
  toWitness _ = Nothing

rewriteEventually ::
  Members '[Modal, Logic, NonDet] effs =>
  Witness 'WitEventually ctx effs Bool ->
  State RewriteContext (ModalQ ctx effs Bool)
rewriteEventually (WEventually modal union k)
  | Just (WEventually modal' union' k') <- toWitness modal = do
    setModality ModalityEventually
    return (ModalQ (MetaUpUntil (ConstQ True) modal') union' k')
  | Just (WAlways modal' union' k') <- toWitness modal
    , Just (WEventually _ _ _) <- toWitness modal' = do
    setModality ModalityAlways
    return (ModalQ (MetaAlways modal') union' k')
  | otherwise = do
    setModality ModalityEventually
    modal' <- normalize modal
    return (ModalQ (MetaUpUntil (ConstQ True) modal') union k)

-- -------------------------------------------------------------------------------------------------

instance HasWitness 'WitForall where
  data Witness 'WitForall ctx effs a where
    WForall ::
      [ModalQ ctx effs b] ->
      Union ctx effs a ->
      (a -> Lang ctx effs b) ->
      Witness 'WitForall ctx effs b

  toWitness (ModalQ (MetaForall modals) union k) = Just (WForall modals union k)
  toWitness _ = Nothing

rewriteForall ::
  Members '[Modal, Logic, NonDet] effs =>
  Witness 'WitForall ctx effs Bool ->
  State RewriteContext (ModalQ ctx effs Bool)
rewriteForall (WForall modals _ _) =
  foldrM (\x y -> liftA2 freshConjunctQ (normalize x) (normalize y)) (ConstQ True) modals

-- -------------------------------------------------------------------------------------------------

instance HasWitness 'WitExists where
  data Witness 'WitExists ctx effs a where
    WExists ::
      [ModalQ ctx effs b] ->
      Union ctx effs a ->
      (a -> Lang ctx effs b) ->
      Witness 'WitExists ctx effs b

  toWitness (ModalQ (MetaExists modals) union k) = Just (WExists modals union k)
  toWitness _ = Nothing

rewriteExists ::
  Members '[Modal, Logic, NonDet] effs =>
  Witness 'WitExists ctx effs Bool ->
  State RewriteContext (ModalQ ctx effs Bool)
rewriteExists (WExists modals _ _) =
  foldrM (\x y -> liftA2 freshDisjunctQ (normalize x) (normalize y)) (ConstQ False) modals

-- -------------------------------------------------------------------------------------------------

instance HasWitness 'WitComplement where
  data Witness 'WitComplement ctx effs a where
    WComplement ::
      ModalQ ctx effs b ->
      Union ctx effs a ->
      (a -> Lang ctx effs b) ->
      Witness 'WitComplement ctx effs b

  toWitness (ModalQ (MetaComplement modal) union k) = Just (WComplement modal union k)
  toWitness _ = Nothing

rewriteComplement ::
  Members '[Modal, Logic, NonDet] effs =>
  Witness 'WitComplement ctx effs Bool ->
  State RewriteContext (ModalQ ctx effs Bool)
rewriteComplement (WComplement modal union k)
  | Just (WComplement modal' _ _) <- toWitness modal =
    return modal'
  | Just (WAlways modal' _ _) <- toWitness modal = do
    setModality ModalityEventually
    return (freshEventuallyQ (freshComplementQ modal'))
  | Just (WEventually modal' _ _) <- toWitness modal = do
    setModality ModalityAlways
    return (freshAlwaysQ (freshComplementQ modal'))
  | Just (WForall modals' union' k') <- toWitness modal =
    return (ModalQ (MetaExists modals') union' k')
  | Just (WConjunct lhs rhs _ _) <- toWitness modal =
    return (freshDisjunctQ (freshComplementQ lhs) (freshComplementQ rhs))
  | Just (WDisjunct lhs rhs _ _) <- toWitness modal =
    return (freshConjunctQ (freshComplementQ lhs) (freshComplementQ rhs))
  | ConstQ True <- modal =
    return (ConstQ False)
  | ConstQ False <- modal =
    return (ConstQ True)
  | otherwise = do
    modal' <- normalize modal
    return (ModalQ (MetaComplement modal') union k)

-- -------------------------------------------------------------------------------------------------

instance HasWitness 'WitConjunct where
  data Witness 'WitConjunct ctx effs a where
    WConjunct ::
      ModalQ ctx effs b ->
      ModalQ ctx effs b ->
      Union ctx effs a ->
      (a -> Lang ctx effs b) ->
      Witness 'WitConjunct ctx effs b

  toWitness (ModalQ (MetaConjunct lhs rhs) union k) = Just (WConjunct lhs rhs union k)
  toWitness _ = Nothing

rewriteConjunct ::
  Members '[Modal, Logic, NonDet] effs =>
  Witness 'WitConjunct ctx effs Bool ->
  State RewriteContext (ModalQ ctx effs Bool)
rewriteConjunct (WConjunct lhs rhs union k) =
  gets rewriteModality >>= \case
    ModalityAlways
      | Just (WAlways p _ _) <- toWitness lhs
        , Just (WAlways q _ _) <- toWitness rhs ->
        alwaysThrough p q
      | otherwise -> distribute lhs rhs
    ModalityUpUntil
      | Just (WAlways p _ _) <- toWitness lhs
        , Just (WAlways q _ _) <- toWitness rhs -> do
        setModality ModalityAlways
        alwaysThrough p q
      | otherwise -> distribute lhs rhs
    _
      | Just (WAlways p _ _) <- toWitness lhs
        , Just (WAlways q _ _) <- toWitness rhs -> do
        setModality ModalityAlways
        alwaysThrough p q
      | ConstQ True <- lhs -> return rhs
      | ConstQ True <- rhs -> return lhs
      | otherwise -> distribute lhs rhs
  where
    alwaysThrough p q = return (freshAlwaysQ (freshConjunctQ p q))

    distribute p q = do
      p' <- normalize p
      q' <- normalize q
      return (ModalQ (MetaConjunct p' q') union k)

-- -------------------------------------------------------------------------------------------------

instance HasWitness 'WitDisjunct where
  data Witness 'WitDisjunct ctx effs a where
    WDisjunct ::
      ModalQ ctx effs b ->
      ModalQ ctx effs b ->
      Union ctx effs a ->
      (a -> Lang ctx effs b) ->
      Witness 'WitDisjunct ctx effs b

  toWitness (ModalQ (MetaDisjunct lhs rhs) union k) = Just (WDisjunct lhs rhs union k)
  toWitness _ = Nothing

rewriteDisjunct ::
  Members '[Modal, Logic, NonDet] effs =>
  Witness 'WitDisjunct ctx effs Bool ->
  State RewriteContext (ModalQ ctx effs Bool)
rewriteDisjunct (WDisjunct lhs rhs union k) =
  gets rewriteModality >>= \case
    ModalityAlways
      | Just (WEventually p _ _) <- toWitness lhs
        , Just (WEventually q _ _) <- toWitness rhs -> do
        setModality ModalityEventually
        eventuallyThrough p q
      | otherwise -> distribute lhs rhs
    ModalityUpUntil
      | Just (WEventually p _ _) <- toWitness lhs
        , Just (WEventually q _ _) <- toWitness rhs -> do
        setModality ModalityEventually
        eventuallyThrough p q
      | otherwise -> distribute lhs rhs
    _
      | Just (WEventually p _ _) <- toWitness lhs
        , Just (WEventually q _ _) <- toWitness rhs -> do
        setModality ModalityEventually
        eventuallyThrough p q
      | otherwise -> distribute lhs rhs
  where
    eventuallyThrough p q = return (freshEventuallyQ (freshDisjunctQ p q))

    distribute p q = do
      p' <- normalize p
      q' <- normalize q
      return (ModalQ (MetaDisjunct p' q') union k)
