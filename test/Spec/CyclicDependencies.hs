module Spec.CyclicDependencies
  ( prop_cyclic,
  )
where

import Control.Exception
  ( BlockedIndefinitelyOnSTM (BlockedIndefinitelyOnSTM),
    fromException,
  )
import Control.Monad.IO.Class (liftIO)
import Hedgehog
  ( Property,
    failure,
    property,
    success,
    withTests,
  )

import Control.Monad.Mapping (MappingT, Var (Var), getPrime)
import Data.Type.HList (HList, HListT (HNil, (:.:)), pattern (:<:))
import Language.Spectacle.Spec (runSpec)

-- -----------------------------------------------------------------------------

type Cyclic = '[Var "x" Int]

initState :: HList Cyclic
initState = Var 0 :<: HNil

-- | This next state relation forms a cyclic dependency:
--
-- * The new value of "x" depends on the new value of "x".
nextState :: HListT (MappingT Cyclic IO) Cyclic
nextState = getPrime @"x" :.: HNil

-- | Test if 'Spec' is able to detect the cyclic dependency and throw the
-- appropriate STM exception.
prop_cyclic :: Property
prop_cyclic = withTests 1 . property $ do
  liftIO (runSpec initState nextState) >>= \case
    Left err -> case fromException err of
      Just BlockedIndefinitelyOnSTM -> success
      Nothing -> failure
    Right _ -> failure
