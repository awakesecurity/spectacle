module Checker.Diehard
  ( prop,
  )
where

import Control.Monad.IO.Class (liftIO)
import Hedgehog
  ( Property,
    annotateShow,
    assert,
    failure,
    property,
    success,
    withTests,
  )

import Control.Monad.Mapping (MappingT, Var (Var), getPrime, getVar)
import Data.Type.HList (HList, HListT (HNil, (:.:)), pattern (:<:))
import Language.Spectacle.Checker (modelCheck, successes)

-- -----------------------------------------------------------------------------

type Diehard =
  '[ Var "smallJug" Int
   , Var "bigJug" Int
   ]

initState :: HList Diehard
initState = Var 0 :<: Var 0 :<: HNil

emptySmallJug :: HListT (MappingT Diehard IO) Diehard
emptySmallJug =
  do return (Var 0)
  :.: do getVar @"bigJug"
    :.: HNil

emptyBigJug :: HListT (MappingT Diehard IO) Diehard
emptyBigJug =
  do getVar @"smallJug"
  :.: do return (Var 0)
    :.: HNil

fillSmallJug :: HListT (MappingT Diehard IO) Diehard
fillSmallJug =
  do return (Var 3)
  :.: do getVar @"bigJug"
    :.: HNil

fillBigJug :: HListT (MappingT Diehard IO) Diehard
fillBigJug =
  do getVar @"smallJug"
  :.: do return (Var 5)
    :.: HNil

smallToBig :: HListT (MappingT Diehard IO) Diehard
smallToBig =
  do
    Var smallJug <- getVar @"smallJug"
    Var bigJug <- getVar @"bigJug"
    Var bigJug' <- getPrime @"bigJug"
    return (Var $ smallJug - (bigJug' - bigJug))
    :.: do
      Var smallJug <- getVar @"smallJug"
      Var bigJug <- getVar @"bigJug"
      return (Var $ min (bigJug + smallJug) 5)
    :.: HNil

bigToSmall :: HListT (MappingT Diehard IO) Diehard
bigToSmall =
  do
    Var smallJug <- getVar @"smallJug"
    Var bigJug <- getVar @"bigJug"
    return (Var $ min (bigJug + smallJug) 3)
    :.: do
      Var smallJug <- getVar @"smallJug"
      Var smallJug' <- getPrime @"smallJug"
      Var bigJug <- getVar @"bigJug"
      return (Var $ bigJug - (smallJug' - smallJug))
    :.: HNil

prop :: Property
prop = withTests 1 . property $ do
  let goal :: HList Diehard -> Bool
      goal (_ :<: Var bigJug :<: _) = bigJug == 4

      invar :: HList Diehard -> Bool
      invar (Var smallJug :<: Var bigJug :<: _) =
        0 <= smallJug && smallJug <= 3 && 0 <= bigJug && bigJug <= 5

  result <-
    liftIO
      ( modelCheck
          [ emptySmallJug
          , emptyBigJug
          , fillSmallJug
          , fillBigJug
          , smallToBig
          , bigToSmall
          ]
          invar
          goal
          initState
      )

  case result of
    Left fails -> do
      mapM_ annotateShow fails
      failure
    Right ckrSt -> do
      let found = successes ckrSt

          expected :: [HList Diehard]
          expected =
            [ Var 0 :<: Var 4 :<: HNil
            , Var 3 :<: Var 4 :<: HNil
            ]

      assert (all (`elem` expected) found)
      success
