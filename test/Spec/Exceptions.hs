module Spec.Exceptions
  ( prop_exceptions,
  )
where

import Control.Exception
  ( ArithException (DivideByZero),
    fromException,
  )
import Control.Monad.IO.Class (liftIO)
import Hedgehog
  ( Property,
    annotate,
    annotateShow,
    failure,
    property,
    success,
    withTests,
  )

import Control.Monad.Mapping (MappingT, Var (Var), getPrime)
import Data.Type.HList (HList, HListT (HNil, (:.:)), pattern (:<:))
import Language.Spectacle.Spec (runSpec)

-- -----------------------------------------------------------------------------

type Sharing = '[Var "x" Int, Var "y" Int]

initState :: HList Sharing
initState = Var 0 :<: Var 0 :<: HNil

-- | This next-state relation tests if Spec can catch exceptions thrown inside
-- of a transition.
nextState :: HListT (MappingT Sharing IO) Sharing
nextState =
  return (Var $! 1 `div` 0)
    :.: do
      Var x' <- getPrime @"x"
      return (Var (x' + 1))
    :.: HNil

prop_exceptions :: Property
prop_exceptions = withTests 1 . property $ do
  liftIO (runSpec initState nextState) >>= \case
    Left err -> case fromException err of
      Just DivideByZero -> success
      Just _ -> do
        annotate "Any exception besides 'DivideByZero' is unexpected."
        annotate "exception thrown:"
        annotateShow err
        failure
      Nothing -> do
        annotate "Any exception besides 'DivideByZero' is unexpected."
        annotate "exception thrown:"
        annotateShow err
        failure
    Right xs -> do
      annotate "It should be impossible for 'Spec' to successfully evaluate this."
      annotate "evaluates to:"
      annotateShow xs
      annotate "An exception here most likely means that the value was not WHNF."
      failure
