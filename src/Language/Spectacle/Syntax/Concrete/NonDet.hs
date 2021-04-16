module Language.Spectacle.Syntax.Concrete.NonDet
  ( NonDet (Choose, Empty),
    Effect (NonDet),
    oneOf,
    foldMapA,
    runNonDetA,
  )
where

import Control.Applicative (Alternative (empty, (<|>)), Applicative (liftA2))
import Control.Monad (join)
import Data.Coerce (coerce)
import Data.Monoid (Alt (Alt, getAlt))
import Data.Void (absurd)

import qualified Data.Functor.Loom as Loom
import Language.Spectacle.Lang
  ( Effect,
    Lang (..),
    Union (Op, Scoped),
    decomposeOp,
    decomposeS,
  )
import Language.Spectacle.Syntax.Concrete.NonDet.Internal
  ( Effect (NonDet),
    NonDet (Choose, Empty),
  )

-- -----------------------------------------------------------------------------

oneOf :: (Foldable t, Alternative m) => t a -> m a
oneOf = foldMapA pure

foldMapA :: (Foldable t, Alternative m) => (a -> m b) -> t a -> m b
foldMapA f = getAlt . foldMap (Alt . f)

runNonDetA ::
  forall f ctx effs a.
  Alternative f =>
  Lang ctx (NonDet ': effs) a ->
  Lang ctx effs (f a)
runNonDetA = \case
  Pure x -> pure (pure x)
  Yield (Op op) k -> case decomposeOp op of
    Left other -> Yield (Op other) k'
    Right Empty -> pure empty
    Right Choose -> liftA2 (<|>) (k' True) (k' False)
    where
      k' = runNonDetA . k
  Yield (Scoped s l) k -> case decomposeS s of
    Left other -> Yield (Scoped other weaveNonDetA) (runNonDetA . (>>= k))
    Right bot -> absurd (coerce bot)
    where
      weaveNonDetA = Loom.weave (pure ()) (runNonDetA . join) l
