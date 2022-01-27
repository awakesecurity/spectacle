-- | The 'Fingerprint' data type.
--
-- @since 0.1.0.0
module Data.Fingerprint
  ( -- * Fingerprints
    Fingerprint (Fingerprint),
    getFingerprint,

    -- ** Construction
    fingerprintRec,

    -- ** Conversions
    showAsHex,
  )
where

import Data.Bits (Bits (rotateR, (.&.)))
import Data.Hashable (Hashable, hash)
import Prettyprinter (Pretty, pretty)

import Data.Type.Rec (Rec)

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'Fingerprint' is a 32-bit hash used for uniquely identifying worlds while model checking.
--
-- @since 0.1.0.0
newtype Fingerprint = Fingerprint {getFingerprint :: Int}
  deriving stock (Eq, Ord)
  deriving (Enum, Hashable, Integral, Num, Real) via Int

-- | @since 0.1.0.0
instance Show Fingerprint where
  show (Fingerprint fp) = "0x" ++ showAsHex fp
  {-# INLINE show #-}

-- | @since 0.1.0.0
instance Pretty Fingerprint where
  pretty = pretty . show
  {-# INLINE pretty #-}

-- | Constructs a probabilistically unique 'Fingerprint' from the given world.
--
-- @since 0.1.0.0
fingerprintRec :: Hashable (Rec ctx) => Rec ctx -> Fingerprint
fingerprintRec world = Fingerprint (hash world)

-- | Converts an 'Int' to a hexdecimal string without a strictly positive constraint and without the preformance hit of
-- the analogous prelude function 'showHex'.
--
-- @since 0.1.0.0
showAsHex :: Int -> String
showAsHex = go 0
  where
    go :: Int -> Int -> String
    go i n
      | i < 8 = fastToHexEnum (rotateR n (4 * i) .&. 0xF) : go (i + 1) n
      | otherwise = []

-- | Converts an 'n :: Int' in the interval [0, 15] to @['0' .. '9']@ if @n <= 9@, otherwise @n@ is mapped to
-- @['a' .. 'f']@. Used to quickly convert 'Int' to a hexdecimal string.
--
-- Note 'fastToHexEnum' is implemented via 'toEnum' so it is as fast as it is unsafe.
--
-- @since 0.1.0.0
fastToHexEnum :: Int -> Char
fastToHexEnum n
  -- The integer range for the enums @['0' .. '9'] :: 'String'@ is @[48 .. 56] :: ['Int']@.
  | n <= 9 = toEnum (n + 48)
  -- The integer range for the enums @['a' .. 'f'] :: 'String'@ is @[97 .. 102] :: ['Int']@, so to construct the map
  -- [10 .. 15] -> [97 .. 102] we subtract have to subtract 10 on both sides, hence adding 87.
  | otherwise = toEnum (n + 87)
