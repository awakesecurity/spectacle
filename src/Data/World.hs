{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.World
  ( -- * Worlds
    World (World),

    -- ** Construction
    makeWorld,

    -- ** Lenses
    worldFingerprint,
    worldValues,
  )
where

import Data.Hashable (Hashable (hashWithSalt))
import Lens.Micro (Lens', SimpleGetter, lens, to)

import Data.Type.Rec (Rec)
import Language.Spectacle.Checker.Fingerprint (Fingerprint (Fingerprint), fingerprintRec)

-- ---------------------------------------------------------------------------------------------------------------------

-- | The 'World' data type is a 'Rec', which is used to represent the concrete values of a model's state, paired with
-- it's 'Fingerprint' which has much faster preformance charateristics for comparison.
--
-- @since 0.1.0.0
data World ctxt = World
  { _worldFingerprint :: {-# UNPACK #-} !Fingerprint
  , _worldValues :: Rec ctxt
  }

-- | @since 0.1.0.0
instance Eq (World ctxt) where
  World fp1 _ == World fp2 _ = fp1 == fp2
  {-# INLINE (==) #-}

-- | @since 0.1.0.0
instance Ord (World ctxt) where
  World fp1 _ `compare` World fp2 _ = fp1 `compare` fp2
  {-# INLINE compare #-}

-- | @since 0.1.0.0
instance Show (Rec ctxt) => Show (World ctxt) where
  show (World fp w) = "<<" ++ show fp ++ ":" ++ show w ++ ">>"
  {-# INLINE show #-}

-- | @since 0.1.0.0
instance Hashable (World ctxt) where
  hashWithSalt salt (World (Fingerprint fp) _) = hashWithSalt salt fp
  {-# INLINE hashWithSalt #-}

-- | Constructs a 'World' type from the given 'Rec'.
--
-- @since 0.1.0.0
makeWorld :: Hashable (Rec ctxt) => Rec ctxt -> World ctxt
makeWorld w = World (fingerprintRec w) w
{-# INLINE makeWorld #-}

-- | Lens focusing on a 'World's fingerprint.
--
-- @since 0.1.0.0
worldFingerprint :: Lens' (World ctxt) Fingerprint
worldFingerprint = lens _worldFingerprint \World {..} x -> World {_worldFingerprint = x, ..}
{-# INLINE worldFingerprint #-}

-- | Lens focusing on the 'Rec' holding the concrete values of a 'World'.
--
-- @since 0.1.0.0
worldValues :: SimpleGetter (World ctxt) (Rec ctxt)
worldValues = to _worldValues
{-# INLINE worldValues #-}
