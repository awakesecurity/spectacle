{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.NonDet.Internal
  ( NonDet (Empty, Choose),
    Effect (MSplit),
  )
where

import Language.Spectacle.Lang.Member (Member)
import Language.Spectacle.Lang.Scoped (Effect)

-- ---------------------------------------------------------------------------------------------------------------------

data NonDet a where
  Empty :: NonDet a
  Choose :: NonDet Bool

data instance Effect NonDet m a where
  MSplit :: (m ~ f ctx effs, Member NonDet effs) => m a -> Effect NonDet m (Maybe (m a, m a))
