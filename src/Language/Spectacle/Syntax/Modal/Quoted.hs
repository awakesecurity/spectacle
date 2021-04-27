{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Spectacle.Syntax.Modal.Quoted
  ( -- * Quoted Temporal Formula
    Quoted (..),
    runQuoted,
    quoting,
    lowerQuoted,

    -- * Quoted Terms
    ModalQ (..),
    reifyQ,
    ModalMeta (..),
  )
where

import Control.Monad.Codensity

import Language.Spectacle.Lang

-- -------------------------------------------------------------------------------------------------

newtype Quoted ctx effs effs' a where
  Quoted :: Codensity (Lang ctx effs) (ModalQ ctx effs' a) -> Quoted ctx effs effs' a

runQuoted :: Quoted ctx effs effs' a -> Lang ctx effs (ModalQ ctx effs' a)
runQuoted (Quoted codensity) = runCodensity codensity pure

quoting ::
  (forall b. (ModalQ ctx effs' a -> Lang ctx effs b) -> Lang ctx effs b) ->
  Quoted ctx effs effs' a
quoting cont = Quoted (Codensity cont)

lowerQuoted :: Quoted ctx effs effs' a -> Lang ctx effs (ModalQ ctx effs' a)
lowerQuoted (Quoted codensity) = lowerCodensity codensity

-- -------------------------------------------------------------------------------------------------

data ModalQ ctx effs a where
  ConstQ :: a -> ModalQ ctx effs a
  ModalQ :: ModalMeta ctx effs b -> Union ctx effs a -> (a -> Lang ctx effs b) -> ModalQ ctx effs b

reifyQ :: ModalQ ctx effs a -> Lang ctx effs a
reifyQ = \case
  ConstQ x -> pure x
  ModalQ _ union k -> Yield union k

instance Functor (ModalQ ctx effs) where
  fmap f = \case
    ConstQ x -> ConstQ (f x)
    ModalQ meta union k ->
      let meta' = case meta of
            MetaAlways m -> MetaAlways (fmap f m)
            MetaUpUntil lhs rhs -> MetaUpUntil (fmap f lhs) (fmap f rhs)
            MetaForall ms -> MetaForall (map (fmap f) ms)
            MetaExists ms -> MetaExists (map (fmap f) ms)
            MetaComplement m -> MetaComplement (fmap f m)
            MetaConjunct lhs rhs -> MetaConjunct (fmap f lhs) (fmap f rhs)
            MetaDisjunct lhs rhs -> MetaDisjunct (fmap f lhs) (fmap f rhs)
       in ModalQ meta' union (fmap f . k)

instance Show a => Show (ModalQ ctx effs a) where
  show = \case
    ConstQ x -> "(ConstQ " ++ show x ++ ")"
    ModalQ meta _ _ -> show meta

-- -------------------------------------------------------------------------------------------------

data ModalMeta ctx effs a where
  MetaAlways :: ModalQ ctx effs a -> ModalMeta ctx effs a
  MetaUpUntil :: ModalQ ctx effs a -> ModalQ ctx effs a -> ModalMeta ctx effs a
  MetaForall :: [ModalQ ctx effs b] -> ModalMeta ctx effs b
  MetaExists :: [ModalQ ctx effs b] -> ModalMeta ctx effs b
  MetaComplement :: ModalQ ctx effs a -> ModalMeta ctx effs a
  MetaConjunct :: ModalQ ctx effs a -> ModalQ ctx effs a -> ModalMeta ctx effs a
  MetaDisjunct :: ModalQ ctx effs a -> ModalQ ctx effs a -> ModalMeta ctx effs a

instance Functor (ModalMeta ctx effs) where
  fmap f = \case
    MetaAlways m -> MetaAlways (fmap f m)
    MetaUpUntil m n -> MetaUpUntil (fmap f m) (fmap f n)
    MetaForall metas -> MetaForall (map (fmap f) metas)
    MetaExists metas -> MetaExists (map (fmap f) metas)
    MetaComplement m -> MetaComplement (fmap f m)
    MetaConjunct m n -> MetaConjunct (fmap f m) (fmap f n)
    MetaDisjunct m n -> MetaDisjunct (fmap f m) (fmap f n)

instance Show a => Show (ModalMeta ctx effs a) where
  show = \case
    MetaAlways meta -> "(AlwaysQ " ++ show meta ++ ")"
    MetaUpUntil lhs rhs -> "(UpUntilQ " ++ show lhs ++ " " ++ show rhs ++ ")"
    MetaForall metas -> "(ForallQ " ++ show metas ++ ")"
    MetaExists metas -> "(ExistsQ " ++ show metas ++ ")"
    MetaComplement meta -> "(Complement " ++ show meta ++ ")"
    MetaConjunct lhs rhs -> "(ConjunctQ " ++ show lhs ++ " " ++ show rhs ++ ")"
    MetaDisjunct lhs rhs -> "(DisjunctQ " ++ show lhs ++ " " ++ show rhs ++ ")"
