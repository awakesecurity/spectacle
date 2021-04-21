{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.NonDet.Internal
  ( NonDet (Empty, Choose),
    Effect (NonDet),
  )
where

import Data.Void (Void)

import Language.Spectacle.Lang.Scoped (Effect)

-- -----------------------------------------------------------------------------

data NonDet a where
  Empty :: NonDet a
  Choose :: NonDet Bool

newtype instance Effect NonDet m a where
  NonDet :: Void -> Effect NonDet m a
