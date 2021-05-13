module Control.Natural
  ( type (~>),
  )
where

import Data.Kind (Type)

-- -------------------------------------------------------------------------------------------------

infix 0 ~>
type (~>) :: (Type -> Type) -> (Type -> Type) -> Type
type f ~> g = forall x. f x -> g x
