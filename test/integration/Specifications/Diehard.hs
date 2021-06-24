{-# LANGUAGE OverloadedLabels #-}

module Specifications.Diehard where

import Data.Foldable
import qualified Data.HashMap.Strict as HashMap

import Data.Type.Rec
import Language.Spectacle.AST.Action
import Language.Spectacle.AST.Initial
import Language.Spectacle.AST.Invariant
import Language.Spectacle.AST.Terminate
import Language.Spectacle.Exception
import Language.Spectacle.Spec
import Language.Spectacle.Spec.Base
import Language.Spectacle.Spec.Model.Base
import Language.Spectacle.Syntax

-- -------------------------------------------------------------------------------------------------

type Diehard =
  '[ "smallJug" # Int
   , "bigJug" # Int
   ]
  
initial :: Initial Diehard ()
initial = do
  #smallJug `define` pure 0
  #bigJug `define` pure 0

next :: Action Diehard Bool
next = do
  fillSmallJug
    \/ fillBigJug
    \/ emptySmallJug
    \/ emptyBigJug
    \/ smallToBig
    \/ bigToSmall
  where
    fillSmallJug = #smallJug .= return 3 >> return True

    fillBigJug = #bigJug .= return 5 >> return True

    emptySmallJug = #smallJug .= return 0 >> return True

    emptyBigJug = #bigJug .= return 0 >> return True

    smallToBig = do
      smallJug <- plain #smallJug
      bigJug <- plain #bigJug
      #smallJug .= do
        bigJug' <- prime #bigJug
        return (smallJug - (bigJug' - bigJug))
      #bigJug .= return (min (bigJug + smallJug) 5)
      return True

    bigToSmall = do
      smallJug <- plain #smallJug
      bigJug <- plain #bigJug
      #smallJug .= return (min (bigJug + smallJug) 3)
      #bigJug .= do
        smallJug' <- prime #smallJug
        return (bigJug - (smallJug' - smallJug))
      return True

invariant :: Invariant Diehard Bool
invariant = do
  always smallJugBounds /\ always bigJugBounds
  where
    smallJugBounds = do
      smallJug <- plain #smallJug
      return (0 <= smallJug && smallJug <= 3)

    bigJugBounds = do
      bigJug <- plain #bigJug
      return (0 <= bigJug && bigJug <= 5)

terminate :: Terminate Diehard Bool
terminate = do
  bigJug <- plain #bigJug
  return (bigJug == 4)

check :: IO ()
check = case doModelCheck initial next invariant (Just terminate) Unfair of
  (Left exc, _) -> do
    putStrLn "model check failed:"
    print exc
  (Right _, st) -> do
    putStrLn "model success, final state:"
    forM_ (HashMap.toList (_coverageMap st)) \(world, info) ->
      putStrLn (show world ++ ", info: " ++ show info)
