{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}

module Specifications.SimpleClock where

import Data.Type.Rec (type (#))
import Language.Spectacle
  ( always,
    define,
    doModelCheck,
    eventually,
    plain,
    prime,
    (.=),
    (/\),
    (==>),
  )
import Language.Spectacle.AST (Action, Initial, Invariant)
import Language.Spectacle.Spec.Base (Fairness (WeaklyFair))

-- ---------------------------------------------------------------------------------------------------------------------

type SimpleClock =
  '[ "hours" # Int
   , "minutes" # Int
   ]

initial :: Initial SimpleClock ()
initial = do
  #hours `define` return 0
  #minutes `define` return 0

action :: Action SimpleClock Bool
action = do
  hours <- plain #hours
  minutes <- plain #minutes

  #hours .= do
    minutes' <- prime #minutes
    if minutes' == 0
      then return ((hours + 1) `mod` 24)
      else return hours

  #minutes
    .= if minutes == 59
      then return 0
      else return (minutes + 1)

  return True

invariant :: Invariant SimpleClock Bool
invariant = do
  hours <- plain #hours
  minutes <- plain #minutes

  always (return (0 <= hours && hours <= 23))
    /\ always (return (0 <= hours && minutes <= 59))
    /\ (return (minutes == 0) ==> eventually (return (hours == 23)))

check :: IO ()
check = do
  case doModelCheck initial action invariant Nothing WeaklyFair of
    (Left exc, _) -> putStrLn $ "failed with: " ++ show exc
    _ -> putStrLn "success"
