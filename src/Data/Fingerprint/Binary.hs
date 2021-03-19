{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | 'Data.Fingerprint' bit twiddling.
--
-- @since 0.1.0.0
module Data.Fingerprint.Binary
  ( Binary (..),
    Hash,
    Bitmap,
    bitsPerSubkey,
    subkeyMask,
    maskIndex,
    bitmask,
  )
where

import Data.Bits
  ( Bits,
    FiniteBits,
    bit,
    finiteBitSize,
    popCount,
    shiftR,
    (.&.),
  )
import Data.Char (intToDigit)
import Data.Word (Word16, Word32)
import Numeric (showIntAtBase)

-- -----------------------------------------------------------------------------

-- | A wrapper over integral types that is equipped with binary pretty printing.
--
-- @since 0.1.0.0
newtype Binary a = Binary a
  deriving
    ( Enum
    , Eq
    , Ord
    , Integral
    , Num
    , Real
    , Bits
    , FiniteBits
    )

-- | A 32-bit 'Fingerprint' hash, from 'Data.Hashable'.
--
-- @since 0.1.0.0
newtype Hash = Hash (Binary Word32)
  deriving
    ( Enum
    , Eq
    , Ord
    , Integral
    , Num
    , Real
    , Show
    , Bits
    , FiniteBits
    )

-- | The inner bitmask used by 'Fingerprint' is 16 bits.
--
-- @since 0.1.0.0
newtype Bitmap = Bitmap (Binary Word16)
  deriving
    ( Enum
    , Eq
    , Ord
    , Integral
    , Num
    , Real
    , Show
    , Bits
    , FiniteBits
    )

-- | @since 0.1.0.0
instance (FiniteBits a, Integral a, Show a) => Show (Binary a) where
  show bin =
    let str = showIntAtBase 2 intToDigit bin ""
        size = finiteBitSize bin
     in replicate (size - length str) '0' <> str

-- | The size of a bitmask in 'Fingerprint' is @2^n@ where @n = bitsPerSubkey@.
--
-- @since 0.1.0.0
bitsPerSubkey :: Int
bitsPerSubkey = 4
{-# INLINE CONLIKE bitsPerSubkey #-}

-- | The subkey mask for 'Fingerprint'.
--
-- >>> subkeyMask
-- >>> 0000000000001111
--
-- @since 0.1.0.0
subkeyMask :: Bitmap
subkeyMask = bit bitsPerSubkey - 1
{-# INLINE CONLIKE subkeyMask #-}

-- | Maps a unique index for a hash between 0 and @2^n@ given a fragment and
-- bitmask of length @2^n@ where @n = 4@. 'Fingerprint' uses this index to sort
-- hashes into a 16-leafed tree.
--
-- @since 0.1.0.0
maskIndex :: Bitmap -> Bitmap -> Int
maskIndex bitmap mask = popCount (bitmap .&. (mask - 1))
{-# INLINE CONLIKE maskIndex #-}

-- | Compute the bitmask at the given shift of some hash.
--
-- @since 0.1.0.0
bitmask :: Hash -> Int -> Bitmap
bitmask hash shift =
  let Bitmap fragment = fromIntegral (shiftR hash shift) .&. subkeyMask
   in bit (fromIntegral fragment)
{-# INLINE CONLIKE bitmask #-}
