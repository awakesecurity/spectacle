module Language.Spectacle.Syntax
  ( -- * Closures
    (.=),

    -- * Errors
    catchE,
    throwE,

    -- * Logic
    conjunct,
    (/\),
    disjunct,
    (\/),
    complement,
    (==>),
    implies,
    (<=>),
    iff,

    -- * Modal Operators
    always,
    eventually,
    upUntil,

    -- * Nondeterminism
    oneOf,

    -- * Variables
    plain,
    prime,

    -- * Quantifiers
    forall,
    exists,

    -- * Enabled
    enabled,
  )
where

import Language.Spectacle.Lang (Lang, Member)
import Language.Spectacle.Syntax.Closure ((.=))
import Language.Spectacle.Syntax.Enabled (enabled)
import Language.Spectacle.Syntax.Error (catchE, throwE)
import Language.Spectacle.Syntax.Logic (Logic, complement, conjunct, disjunct, iff, implies)
import Language.Spectacle.Syntax.Modal (always, eventually, upUntil)
import Language.Spectacle.Syntax.NonDet (oneOf)
import Language.Spectacle.Syntax.Plain (plain)
import Language.Spectacle.Syntax.Prime (prime)
import Language.Spectacle.Syntax.Quantifier (exists, forall)

-- ---------------------------------------------------------------------------------------------------------------------

infixl 5 /\
(/\) :: Member Logic effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
(/\) = conjunct
{-# INLINE (/\) #-}

infixl 5 \/
(\/) :: Member Logic effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
(\/) = disjunct
{-# INLINE (\/) #-}

(==>) :: Member Logic effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
(==>) = implies
{-# INLINE (==>) #-}

(<=>) :: Member Logic effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
(<=>) = iff
{-# INLINE (<=>) #-}
