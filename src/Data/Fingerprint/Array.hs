{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 'Array' is a wrapper over 'Data.Primitive.SmallArray' that is used
-- under the hood by 'Data.Fingerprint' for faster insertion, random access,
-- and garbage collection.
--
-- @since 0.1.0.0
module Data.Fingerprint.Array
  ( Array (..),
    empty,
    singleton,
    (!),
    length,
    updateAt,
    insertAt,
  )
where

import Data.Primitive.SmallArray
  ( SmallArray,
    copySmallArray,
    indexSmallArray,
    newSmallArray,
    runSmallArray,
    sizeofSmallArray,
    writeSmallArray,
  )
import GHC.Exts (IsList)
import Unsafe.Coerce (unsafeCoerce)
import Prelude (Eq, Int, Monoid, Semigroup, Show, pure, ($), (+), (-))

-- -----------------------------------------------------------------------------

-- | A safe wrapper over 'Data.Primitive.SmallArray'.
--
-- @since 0.1.0.0
newtype Array a = Array
  {unsafeUnarray :: SmallArray a}
  deriving
    ( Eq
    , Show
    , IsList
    , Monoid
    , Semigroup
    )

-- | \(\mathcal{O}(1)\). Construct an empty 'Array'.
--
-- @since 0.1.0.0
empty :: Array a
empty = Array (runSmallArray (newSmallArray 0 (unsafeCoerce ())))

-- | \(\mathcal{O}(1)\). Construct a singleton 'Array'.
--
-- @since 0.1.0.0
singleton :: a -> Array a
singleton hash = Array $ runSmallArray (newSmallArray 1 hash)

-- | \(\mathcal{O}(1)\). Index an 'Array'.
--
-- @since 0.1.0.0
(!) :: Array a -> Int -> a
(!) (Array arr) = indexSmallArray arr
{-# INLINE (!) #-}

-- | Query the size of an 'Array'.
--
-- @since 0.1.0.0
length :: Array a -> Int
length (Array arr) = sizeofSmallArray arr
{-# INLINE length #-}

-- | \(\mathcal{O}(1)\). Update the element at an index with the value given.
--
-- @since 0.1.0.0
updateAt :: Array a -> Int -> a -> Array a
updateAt (Array arr) ix hash = Array $ runSmallArray do
  let len = sizeofSmallArray arr
  arr' <- newSmallArray len (unsafeCoerce ())
  copySmallArray arr' 0 arr 0 len
  writeSmallArray arr' ix hash
  pure arr'

-- | \(\mathcal{O}(1)\). Insert an element at the given index, shifting elements
-- after the index to the right. Equivalent to:
--
-- @
-- insert xs n x = take n xs <> [x] <> drop n xs
-- @
--
-- @since 0.1.0.0
insertAt :: Array a -> Int -> a -> Array a
insertAt (Array arr) ix hash = Array $ runSmallArray do
  let len = sizeofSmallArray arr
  arr' <- newSmallArray (len + 1) (unsafeCoerce ())
  copySmallArray arr' 0 arr 0 ix
  copySmallArray arr' (ix + 1) arr ix (len - ix)
  writeSmallArray arr' ix hash
  pure arr'
