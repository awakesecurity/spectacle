module Language.Spectacle.AST.Terminate
  ( -- * Termination Condition Syntax
    type Terminate,
    type TerminateSyntax,

    -- ** Interpreters
    runTerminate,
  )
where

import Data.Function ((&))

import Data.Type.Rec (Rec)
import Language.Spectacle.AST.Terminate.Internal (Terminate, TerminateSyntax)
import Language.Spectacle.Lang (runLang)
import Language.Spectacle.Syntax.Enabled (runEnabled)
import Language.Spectacle.Syntax.Plain (runPlain)

-- ---------------------------------------------------------------------------------------------------------------------

runTerminate :: Bool -> Rec ctx -> Terminate ctx Bool -> Bool
runTerminate isEnabled knowns termination =
  termination
    & runPlain knowns
    & runEnabled isEnabled
    & runLang
{-# INLINE runTerminate #-}
