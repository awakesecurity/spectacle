{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.World
  ( -- * Worlds
    World (World),

    -- ** Construction
    makeWorld,

    -- ** Lenses
    fingerprint,
    worldValues,

    -- ** Pretty Printing
    ppWorldListed,
  )
where

import Data.Hashable (Hashable (hashWithSalt))
import Lens.Micro (Lens', SimpleGetter, lens, to)
import Prettyprinter (Doc, annotate, indent, pretty)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (White, Yellow), color, colorDull)

import Data.Fingerprint (Fingerprint (Fingerprint), fingerprintRec)
import Data.Type.Rec (HasDict, Rec, ppRecListed)

-- ---------------------------------------------------------------------------------------------------------------------

-- | The 'World' data type is a 'Rec', which is used to represent the concrete values of a model's state, paired with
-- it's 'Fingerprint' which has much faster preformance charateristics for comparison.
--
-- @since 1.0.0
data World ctx = World
  { _worldFingerprint :: {-# UNPACK #-} !Fingerprint
  , _worldValues :: Rec ctx
  }

-- | @since 1.0.0
instance Eq (World ctx) where
  World fp1 _ == World fp2 _ = fp1 == fp2
  {-# INLINE (==) #-}

-- | @since 1.0.0
instance Ord (World ctx) where
  World fp1 _ `compare` World fp2 _ = fp1 `compare` fp2
  {-# INLINE compare #-}

-- | @since 1.0.0
instance Show (Rec ctx) => Show (World ctx) where
  show (World fp w) = "<<" ++ show fp ++ ":" ++ show w ++ ">>"
  {-# INLINE show #-}

-- | @since 1.0.0
instance Hashable (World ctx) where
  hashWithSalt salt (World (Fingerprint fp) _) = hashWithSalt salt fp
  {-# INLINE hashWithSalt #-}

-- | Constructs a 'World' type from the given 'Rec'.
--
-- @since 1.0.0
makeWorld :: Hashable (Rec ctx) => Rec ctx -> World ctx
makeWorld w = World (fingerprintRec w) w

-- | Lens focusing on a 'World's fingerprint.
--
-- @since 1.0.0
fingerprint :: Lens' (World ctx) Fingerprint
fingerprint = lens _worldFingerprint \World {..} x -> World {_worldFingerprint = x, ..}

-- | Lens focusing on the 'Rec' holding the concrete values of a 'World'.
--
-- @since 1.0.0
worldValues :: SimpleGetter (World ctx) (Rec ctx)
worldValues = to _worldValues

-- | @'ppWorldListed' world@ lays out a list of documents for each state variable in @world@ using the world fingerprint
-- as a header.
--
-- @since 1.0.0
ppWorldListed :: HasDict Show ctx => World ctx -> [Doc AnsiStyle]
ppWorldListed (World hash values) =
  let hashDoc = annotate (colorDull Yellow) (pretty hash)
      fieldDocs = annotate (color White) <$> ppRecListed values
   in hashDoc : map (indent 2) fieldDocs
