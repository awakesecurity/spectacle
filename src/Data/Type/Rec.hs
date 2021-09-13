{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Rec
  ( -- * Extensible Records
    type Rec,
    pattern RNil,
    pattern RCon,
    constMap,

    -- ** Selectors
    type (.|),
    HasSel,
    getRec,
    setRec,

    -- ** Transformed
    RecT (RNilT, RConT),
    concat,
    getRecT,
    setRecT,
    constMapT,
    fieldMap,
    fields,

    -- * Reflection
    ReflectRow,
    repeatRow,

    -- * Re-exports
    Ascribe,
    type (#),
    Name (Name),
  )
where

import Control.Natural (type (~>))
import Data.Functor.Identity (Identity (Identity))
import Prelude (Monoid (mempty), String, const, show, (.), (<>))

import Data.Ascript (Ascribe, type (#))
import Data.Context (CtxtCat)
import Data.Name (Name (Name))
import Data.Type.Rec.Internal (pattern RCon, pattern RNil, Rec, RecT (RConT, RNilT))
import Data.Type.Rec.Selector
  ( HasSel (getRecT, setRecT),
    ReflectRow (repeatRow),
    getRec,
    setRec,
    type (.|),
  )

-- -------------------------------------------------------------------------------------------------

concat :: RecT f ctxt -> RecT f ctxt' -> RecT f (CtxtCat ctxt ctxt')
concat RNilT ys = ys
concat (RConT name x xs) ys = RConT name x (concat xs ys)

constMap :: Monoid m => (forall s x. Name s -> x -> m) -> Rec ctx -> m
constMap f = constMapT (\name -> f name . Identity)
{-# INLINE constMap #-}

constMapT :: Monoid m => (forall s x. Name s -> f x -> m) -> RecT f ctx -> m
constMapT _ RNilT = mempty
constMapT f (RConT name x r) = f name x <> constMapT f r
{-# INLINE constMapT #-}

fieldMap :: (f ~> g) -> RecT f ctx -> RecT g ctx
fieldMap _ RNilT = RNilT
fieldMap f (RConT name x r) = RConT name (f x) (fieldMap f r)
{-# INLINE fieldMap #-}

fields :: RecT f ctx -> [String]
fields = constMapT \name -> const [show name]
{-# INLINE fields #-}
