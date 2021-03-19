{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 'Fingerprint' is a specialized hash mappend array trie that which hash been
-- stripped down two its spine. For recording "fingerprint" hashes, it
-- outperforms all of the hash datastructures provided by @unordered-containers@
-- and @containers@.
--
-- All of its operations have complexity \(\mathcal{O}(log n)\), however the
-- log-base is 16 so this number grows extremely slow. e.g. a 'Fingerprint' with
-- millions of elements would have an average lookup complexity of about
-- \(\mathcal{O}(5)\).
--
-- @since 0.1.0.0
module Data.Fingerprint
  ( -- * Fingerprint
    Fingerprint,

    -- ** Construction
    empty,
    singleton,
    insert,

    -- ** Querying
    member,
  )
where

import Data.Bits ((.&.), (.|.))
import Data.Hashable (Hashable, hash)
import GHC.Exts (IsList (Item, fromList, toList))

import Data.Fingerprint.Array (Array (Array))
import qualified Data.Fingerprint.Array as Arr
import Data.Fingerprint.Binary
  ( Bitmap,
    Hash,
    bitmask,
    bitsPerSubkey,
    maskIndex,
  )

-- -----------------------------------------------------------------------------

-- | 'Fingerprint' is the spine of a hash mapped array trie. It is specialized
-- for quick insertion and lookup of hashes exclusively.
--
-- @since 0.1.0.0
data Fingerprint
  = None
  | Leaf {-# UNPACK #-} !Hash
  | Many {-# UNPACK #-} !Bitmap {-# UNPACK #-} !(Array Fingerprint)
  deriving (Show)

-- | @since 0.1.0.0
instance IsList Fingerprint where
  type Item Fingerprint = Hash

  fromList xs = foldl (flip insert) empty xs
  {-# INLINE fromList #-}

  toList None = []
  toList (Leaf x) = [x]
  toList (Many _ (Array arr)) = concat (toList (fmap toList arr))
  {-# INLINE toList #-}

-- | Construct an empty 'Fingerprint'.
--
-- @since 0.1.0.0
empty :: Fingerprint
empty = None
{-# INLINE empty #-}

-- | Construct a singleton 'Fingerprint'.
--
-- @since 0.1.0.0
singleton :: Hashable a => a -> Fingerprint
singleton = Leaf . fromIntegral . hash
{-# INLINE singleton #-}

-- | \(\mathcal{O}(log n)\). Query a 'Fingerprint' to see if the provided value
-- has been recorded.
--
-- @since 0.1.0.0
member :: Hashable a => a -> Fingerprint -> Bool
member h = go 0 (fromIntegral . hash $ h)
  where
    go :: Int -> Hash -> Fingerprint -> Bool
    go shift x fp =
      let mask = bitmask x shift
       in case fp of
            None -> False
            Leaf leafHash
              | x == leafHash -> True
              | otherwise -> False
            Many bitmap array
              | bitmap .&. mask == 0 -> False
              | otherwise ->
                let index = maskIndex bitmap mask
                 in go (shift + bitsPerSubkey) x (array Arr.! index)

-- | \(\mathcal{O}(log n)\). Insert a hash into a 'Fingerprint'.
--
-- @since 0.1.0.0
insert :: Hash -> Fingerprint -> Fingerprint
insert = go 0
  where
    go :: Int -> Hash -> Fingerprint -> Fingerprint
    go shift x fp = case fp of
      None -> Leaf x
      Leaf leafHash
        | x == leafHash -> Leaf x
        | otherwise ->
          go shift x (Many (bitmask leafHash shift) (Arr.singleton fp))
      Many bitmap vector ->
        let index = maskIndex bitmap mask
            mask = bitmask x shift
         in if bitmap .&. mask == 0
              then
                let leaf = Leaf x
                    array' = Arr.insertAt vector index leaf
                    bitmap' = bitmap .|. mask
                 in Many bitmap' array'
              else
                let subtree = vector Arr.! index
                    subtree' = go (shift + bitsPerSubkey) x subtree
                    array' = Arr.updateAt vector index subtree'
                 in Many bitmap array'
