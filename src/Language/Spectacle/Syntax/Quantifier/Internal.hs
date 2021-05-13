{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Quantifier.Internal
  ( Quantifier (Quantifier),
    Effect (Forall, Exists),
  )
where

import Data.Void

import Language.Spectacle.Lang

-- ---------------------------------------------------------------------------------------------------------------------

newtype Quantifier :: EffectK where
  Quantifier :: Void -> Quantifier a

data instance Effect Quantifier m a where
  Forall :: [a] -> (a -> m Bool) -> Effect Quantifier m Bool
  Exists :: [a] -> (a -> m Bool) -> Effect Quantifier m Bool
