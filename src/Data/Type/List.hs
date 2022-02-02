{-# LANGUAGE TypeFamilies #-}

-- |
--
-- @since 0.1.0.0
module Data.Type.List
  ( type (++),
  )
where

infix 5 ++

-- ---------------------------------------------------------------------------------------------------------------------

type (++) :: [k] -> [k] -> [k]
type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)
