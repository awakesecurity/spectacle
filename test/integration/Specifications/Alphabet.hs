{-# LANGUAGE OverloadedLabels #-}

-- | Regression test for issue #56 ("Stuttering steps are not inserted for
-- unchanged steps"). The bug-triggering form is the combination of @when (...)
-- (.=)@ with @pure True@: at @letter == 'z'@ the @.=@ is guarded out so the
-- action returns @True@ without writing any primed value, producing a
-- self-looping next-state entry that the model checker used to drop. Do NOT
-- rewrite @next@ to @#letter .= pure (succ letter); pure (letter < 'z')@ — that
-- form was already passing, and substituting it here would silently destroy the
-- regression test.
module Specifications.Alphabet where

import Control.Monad (when)

import Language.Spectacle
  ( Action,
    ActionType (ActionWF),
    Fairness (WeakFair),
    Modality (Eventually),
    Specification (Specification),
    Temporal,
    TemporalType (PropF),
    interaction,
    plain,
    specInit,
    specNext,
    specProp,
    (.=),
    pattern ConF,
    pattern NilF,
    type (#),
  )

-- ---------------------------------------------------------------------------------------------------------------------

interactAlphabetSpec :: IO ()
interactAlphabetSpec = interaction alphabetSpec

-- ---------------------------------------------------------------------------------------------------------------------

type AlphabetSpec =
  Specification
    '["letter" # Char]
    '["next" # 'WeakFair]
    '["isLetterZ" # 'Eventually]

next :: Action '["letter" # Char] Bool
next = do
  letter <- plain #letter
  when (letter < 'z') do
    #letter .= pure (succ letter)
  pure True

isLetterZ :: Temporal '["letter" # Char] Bool
isLetterZ = do
  letter <- plain #letter
  pure (letter == 'z')

alphabetSpec :: AlphabetSpec
alphabetSpec =
  Specification
    { specInit = ConF #letter (pure 'a') NilF
    , specNext = ConF #next (ActionWF next) NilF
    , specProp = ConF #isLetterZ (PropF isLetterZ) NilF
    }
