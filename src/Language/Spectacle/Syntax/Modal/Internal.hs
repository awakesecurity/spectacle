{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Modal.Internal
  ( Modal (Modal),
    Effect (Always, UpUntil),
    Relation (..),
  )
where

import Data.Void (Void)

import Data.Type.Rec (Rec)
import Language.Spectacle.Lang (Members, EffectK, Lang)
import Language.Spectacle.Syntax.Logic.Internal
import Language.Spectacle.Syntax.Plain.Internal

-- -------------------------------------------------------------------------------------------------

newtype Modal :: EffectK where
  Modal :: Void -> Modal a

data instance Effect Modal m b where
  Always :: m Bool -> Effect Modal m Bool
  UpUntil :: m Bool -> m Bool -> Effect Modal m Bool

data Relation ctx = Relation
  { pastState :: Rec ctx
  , futureState :: Rec ctx
  }
