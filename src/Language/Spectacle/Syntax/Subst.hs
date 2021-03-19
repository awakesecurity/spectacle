-- | Substitution syntax.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Subst
  ( Subst (..),
    plain,
    prime,
  )
where

import Language.Spectacle.Lang (Lang, Syntax, send, type (|>))
import Language.Spectacle.Syntax.Name (Name)
import Language.Spectacle.Type.Rec (type (#), type (<:))

-- -----------------------------------------------------------------------------

data Subst :: Syntax where
  Plain :: (nm # a <: cxt, m ~ Lang sig cxt) => Name nm -> Subst m a
  Prime :: (nm # a <: cxt, m ~ Lang sig cxt) => Name nm -> Subst m a

plain :: (nm # a <: cxt, Subst |> sig) => Name nm -> Lang sig cxt a
plain name = send (Plain name)
{-# INLINE plain #-}

prime :: (nm # a <: cxt, Subst |> sig) => Name nm -> Lang sig cxt a
prime name = send (Prime name)
{-# INLINE prime #-}
