module Spec.Sharing
  ( prop_shares_right,
    prop_shares_left,
  )
where

import Control.Monad.IO.Class (liftIO)
import Hedgehog
  ( Property,
    failure,
    property,
    withTests,
    (===),
  )

import Control.Monad.Mapping (MappingT, Var (Var), getPrime, getVar)
import Data.Type.HList (HList, HListT (HNil, (:.:)), pattern (:<:))
import Language.Spectacle.Spec (runSpec)

-- -----------------------------------------------------------------------------

type Sharing =
  '[ Var "x" Int
   , Var "y" Int
   ]

initState :: HList Sharing
initState = Var 0 :<: Var 0 :<: HNil

-- | This next-state relation tests right-associative sharing.
nextStateRight :: HListT (MappingT Sharing IO) Sharing
nextStateRight =
  do
    Var y' <- getPrime @"y"
    return (Var $ y' + 1) -- "x" is the new value of "y" plus one.
    :.: do
      Var y <- getVar @"y"
      return (Var $ y + 5) -- "y" is "y" plus 5.
    :.: HNil

-- | This next-state relation tests left-associative sharing.
nextStateLeft :: HListT (MappingT Sharing IO) Sharing
nextStateLeft =
  do
    Var x <- getVar @"x"
    return (Var $ x + 5) -- "x" is "x" plus 5.
    :.: do
      Var x' <- getPrime @"x"
      return (Var $ x' + 1) -- "y" is the new value of "x" plus one.
    :.: HNil

prop_shares_right :: Property
prop_shares_right = withTests 1 . property $ do
  let expected :: HList Sharing
      expected = Var 6 :<: Var 5 :<: HNil
  liftIO (runSpec initState nextStateRight) >>= \case
    Right result -> result === expected
    Left _ -> failure

prop_shares_left :: Property
prop_shares_left = withTests 1 . property $ do
  let expected :: HList Sharing
      expected = Var 5 :<: Var 6 :<: HNil
  liftIO (runSpec initState nextStateLeft) >>= \case
    Right result -> result === expected
    Left _ -> failure
