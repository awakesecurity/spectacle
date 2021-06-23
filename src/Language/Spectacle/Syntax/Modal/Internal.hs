{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Modal.Internal
  ( Modal (Modal),
    Effect (Always, UpUntil),
  )
where

import Data.Void (Void)

import Language.Spectacle.Lang (Effect, EffectK)

-- -------------------------------------------------------------------------------------------------

newtype Modal :: EffectK where
  Modal :: Void -> Modal a

data instance Effect Modal m b where
  Always :: m Bool -> Effect Modal m Bool
  UpUntil :: m Bool -> m Bool -> Effect Modal m Bool
