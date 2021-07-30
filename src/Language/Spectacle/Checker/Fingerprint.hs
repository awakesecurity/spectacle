-- | The 'Fingerprint' data type.
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.Fingerprint
  ( -- * Fingerprints
    Fingerprint (Fingerprint),

    -- ** Construction
    fingerprintRec,

    -- ** Conversions
    showAsHex,
  )
where

import Data.Bits (Bits (rotateR, (.&.)))
import Data.Hashable (Hashable, hash)

import Data.Type.Rec (Rec)

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'Fingerprint' is a 32-bit hash used for uniquely identifying worlds while model checking.
--
-- @since 0.1.0.0
newtype Fingerprint = Fingerprint Int
  deriving stock (Eq, Ord)
  deriving (Num, Hashable) via Int

-- | @since 0.1.0.0
instance Show Fingerprint where
  show (Fingerprint fp) = "0x" ++ showAsHex fp
  {-# INLINE show #-}

-- | Constructs a probabilistically unique 'Fingerprint' from the given world.
--
-- @since 0.1.0.0
fingerprintRec :: Hashable (Rec ctx) => Rec ctx -> Fingerprint
fingerprintRec world = Fingerprint (hash world)
{-# INLINE CONLIKE fingerprintRec #-}

-- | Converts an 'Int' to a hexdecimal string without a strictly positive constraint and without the preformance hit of
-- the analogous prelude function 'showHex'.
--
-- @since 0.1.0.0
showAsHex :: Int -> String
showAsHex = go 8
  where
    go :: Int -> Int -> String
    go i n
      | i <= 0 = [fastToHexEnum (n .&. 0xF)]
      | otherwise = fastToHexEnum (rotateR n (i * 4) .&. 0xF) : go (i - 1) n
{-# INLINE showAsHex #-}

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
{-# INLINE CONLIKE fastToHexEnum #-}
