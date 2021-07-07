{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Modal.Internal
  ( Modal (Modal),
    Effect (Always, UpUntil),
  )
where

import Data.Void (Void)
import GHC.Stack (SrcLoc)

import Language.Spectacle.Lang (Effect, EffectK)

-- -------------------------------------------------------------------------------------------------

newtype Modal :: EffectK where
  Modal :: Void -> Modal a

data instance Effect Modal m b where
  Always :: {-# UNPACK #-} !(Maybe SrcLoc) -> m Bool -> Effect Modal m Bool
  UpUntil :: {-# UNPACK #-} !(Maybe SrcLoc) -> m Bool -> m Bool -> Effect Modal m Bool
