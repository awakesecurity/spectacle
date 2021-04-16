{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Concrete.NonDet.Internal
  ( NonDet (Empty, Choose),
    Effect (NonDet),
  )
where

import Data.Void

import Language.Spectacle.Lang.Scoped

-- -----------------------------------------------------------------------------

data NonDet a where
  Empty :: NonDet a
  Choose :: NonDet Bool

newtype instance Effect NonDet m a where
  NonDet :: Void -> Effect NonDet m a
