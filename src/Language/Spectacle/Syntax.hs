module Language.Spectacle.Syntax
  ( -- * Syntax
    Name (..),
    reifyName,
    isPrimedName,
  )
where

import Language.Spectacle.Syntax.Ascript (type (#) ((:#:)))
import Language.Spectacle.Syntax.Name
  ( Name (Name),
    isPrimedName,
    reifyName,
  )

-- -----------------------------------------------------------------------------
