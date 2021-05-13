{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Fresh.Internal
  ( Fresh (Fresh),
    Effect (FreshH),
  )
where

import Data.Void

import Language.Spectacle.Lang

-- ---------------------------------------------------------------------------------------------------------------------

data Fresh :: EffectK where
  Fresh :: Fresh Int

newtype instance Effect Fresh m a where
  FreshH :: Void -> Effect Fresh m a
