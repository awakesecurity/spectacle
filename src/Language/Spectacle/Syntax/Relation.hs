module Language.Spectacle.Syntax.Relation
  ( -- * Relations
    type Relation,
    interpretRelation,
  )
where

import Data.Function ((&))

import Language.Spectacle.Effect.NonDet (NonDet, runNonDetAll)
import Language.Spectacle.Lang (Lang, evalLang)
import Language.Spectacle.RTS.Env
  ( Env,
    Environment,
    emptyEnv,
    runEnvironment,
  )
import Language.Spectacle.RTS.Exception
  ( RuntimeError,
    RuntimeException,
    runError,
  )
import Language.Spectacle.Syntax.Closure (Closure, runClosures)
import Language.Spectacle.Type.Rec (Rec)

-- -----------------------------------------------------------------------------

type Relation =
  Lang
    '[ Closure
     , Environment
     , NonDet
     , RuntimeError
     ]

interpretRelation ::
  forall cxt.
  Rec cxt ->
  Relation cxt (Env cxt) ->
  Either RuntimeException [Env cxt]
interpretRelation store relation =
  relation
    & runClosures
    & runEnvironment (emptyEnv store)
    & runNonDetAll
    & runError
    & evalLang store
