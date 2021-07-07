module Language.Spectacle.Interaction
  ( defaultInteraction,
  )
where

import Data.Text.Prettyprint.Doc.Render.Terminal (putDoc)

import Data.Type.Rec (Rec)
import Language.Spectacle.Checker.Metrics (ModelMetrics)
import Language.Spectacle.Checker.Model.MCError (MCError)
import Language.Spectacle.Interaction.Render (renderModelErrorsDoc, renderModelMetrics)

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'defaultIntraction' is an 'IO' action which handles rendering model failures or successful to terminal. Given some
-- specification @spec@
--
-- @
-- main :: IO ()
-- main = defaultIntraction (modelCheck spec)
-- @
--
-- is all that is needed to preform model checks and output the results.
--
-- @since 0.1.0.0
defaultInteraction :: Show (Rec ctx) => Either [MCError ctx] ModelMetrics -> IO ()
defaultInteraction (Left errs) = putDoc =<< renderModelErrorsDoc errs
defaultInteraction (Right metrics) = putDoc (renderModelMetrics metrics)
