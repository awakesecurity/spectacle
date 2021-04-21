{-# LANGUAGE EmptyCase #-}

-- | First-order effect operations.
--
-- @since 0.1.0.0
module Language.Spectacle.Lang.Op
  ( Op (OHere, OThere),
    decomposeOp,
    extractOp,
  )
where

-- -------------------------------------------------------------------------------------------------

-- | 'Op' is an extensible sum inhabited by a first-order effect @eff a@ in @effs@.
--
-- @since 0.1.0.0
data Op effs a where
  OHere :: eff a -> Op (eff ': effs) a
  OThere :: Op effs a -> Op (eff ': effs) a

-- | Orthogonal decomposition for 'Op'. Yields either a proof that the effect @eff@ is not
-- inhabiting the given 'Op' or a constructor for @eff@.
--
-- @since 0.1.0.0
decomposeOp :: Op (eff ': effs) a -> Either (Op effs a) (eff a)
decomposeOp (OHere eff) = Right eff
decomposeOp (OThere op) = Left op
{-# INLINE decomposeOp #-}

-- | A special case of 'decomposeOp'. A singleton sum of @eff@ must be inhabited by @eff@.
--
-- @since 0.1.0.0
extractOp :: Op '[eff] a -> eff a
extractOp (OHere eff) = eff
extractOp (OThere op) = case op of
{-# INLINE extractOp #-}
