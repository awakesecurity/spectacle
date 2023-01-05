-- | The type of natural transformations.
--
-- @since 1.0.0
module Control.Natural
  ( type (~>),
  )
where

import Data.Kind (Type)

-- -------------------------------------------------------------------------------------------------

-- | The type of natural transformations: @f a ~> g b@
--
-- @since 1.0.0
type (~>) :: (Type -> Type) -> (Type -> Type) -> Type
type f ~> g = forall x. f x -> g x

infix 0 ~>
