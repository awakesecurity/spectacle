{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | 'GL' is the temporal comonad defining the global/always modality. Invariant formulas @G p@ are expanded as
-- @G p := p /\ G p'@, 'GL' implements this expansion by taking the cofree constructo '(:<)' to be conjunction.
--
-- Repeated applications of bind/extract on 'GL' is equivalent to traversing semi-open intervals of 'Time' with the
-- infinite represented as 'empty' of the monad transformed by 'GL'.
--
-- @since 0.1.0.0
module Data.Temporal.Global
  ( -- * Global/Always Modality
    GL (GL),
    getGL,

    -- ** Natural Transformations
    endoGL,
  )
where

import Control.Applicative (Applicative (liftA2), Alternative, empty)
import Control.Comonad (Comonad (extend, extract))
import Control.Comonad.Cofree (Cofree ((:<)), ComonadCofree (unwrap))
import Data.Kind (Type)

-- ---------------------------------------------------------------------------------------------------------------------

newtype GL :: (Type -> Type) -> Type -> Type where
  GL :: {getGL :: Cofree w a} -> GL w a
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance Functor w => Comonad (GL w) where
  extract = extract . getGL
  {-# INLINE extract #-}

  extend f (GL w) = GL (extend (f . GL) w)
  {-# INLINE extend #-}

-- | 'endoGL' is the natural transformation defining the global modality. Given a 'GL' we can apply the rules
--
-- @
-- GL (GL f a -> GL f a), by necessity ('pure')
-- GL f (GL f a) -> GL f a, by distribution over arrow
-- GL f a -> f (GL f a), unwrapping 'Cofree',
-- @
--
-- defining G p = p /\ G p' where '(:<)' is analogous to conjunction.
--
-- @since 0.1.0.0
endoGL :: Applicative f => GL f a -> f (GL f a)
endoGL (GL (_ :< ts)) = fmap GL (liftA2 (:<) (fmap extract ts) (fmap unwrap ts))
