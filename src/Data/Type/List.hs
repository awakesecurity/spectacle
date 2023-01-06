{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Data.Type.List
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Utilities for type-level lists.
--
-- @since 0.1.0.0
module Data.Type.List
  ( type (++),
  )
where

-- ---------------------------------------------------------------------------------------------------------------------

-- | Concatenation of type-level lists.
type (++) :: [k] -> [k] -> [k]
type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

infix 5 ++
