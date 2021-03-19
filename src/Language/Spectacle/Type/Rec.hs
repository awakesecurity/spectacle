-- For fourmolu
{-# LANGUAGE TypeApplications #-}

-- | 'Record' is an extensible record type indexed by row of it's fields.
-- Constructing records is done by declaring its 'Field's using spectacle's
-- 'Name' syntax.
--
-- @
-- >>> :set -XOverloadedLabels
-- >>> let myRec = Field \#theBool True :| Field \#theString "teapot" :| RNil
-- @
--
-- Values within a record can then be projected with either 'getRec' or the
-- field selector syntax:
--
-- @
-- >>> #theBool myRec :: Bool
-- >>> True
-- >>> getRec @("theString" # String) myRec
-- @
--
-- @since 0.1.0.0
module Language.Spectacle.Type.Rec
  ( -- * Extensible records
    Rec,

    -- ** Record fields
    Field,
    pattern Field,

    -- ** Modifying Fields
    getRec,
    putRec,
    modifyRec,

    -- * Record transformer
    RecT (..),
    overFields,
    type (<:) (..),

    -- ** Field transformer
    FieldT (..),

    -- * Re-exports
    module Language.Spectacle.Syntax.Ascript,
    module Language.Spectacle.Syntax.Name,
  )
where

import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Kind (Constraint, Type)

import GHC.TypeLits (Symbol)
import Language.Spectacle.Syntax.Ascript (Ascribe, type (#))
import Language.Spectacle.Syntax.Name (Name, inferName)

-- -----------------------------------------------------------------------------

-- | 'Rec' is an extensible record type.
--
-- @since 0.1.0.0
type Rec = RecT Identity

-- | Gets the value of a 'Rec' field by name.
--
-- @since 0.1.0.0
getRec :: nm # a <: row => Rec row -> Name nm -> a
getRec = fmap runIdentity . getRecT
{-# INLINE getRec #-}

-- | Sets a 'Rec's field of the given name to the value @a@.
--
-- @since 0.1.0.0
putRec :: nm # a <: row => Rec row -> Name nm -> a -> Rec row
putRec record nm = putRecT record nm . Identity
{-# INLINE putRec #-}

-- | Modify a 'Rec' field of the given name.
--
-- @since 0.1.0.0
modifyRec :: nm # a <: row => Rec row -> Name nm -> (a -> a) -> Rec row
modifyRec record nm f = putRec record nm (f (getRec record nm))
{-# INLINE modifyRec #-}

-- | 'Field' is the type of fields in an extensible record.
--
-- @since 0.1.0.0
type Field nm = FieldT nm Identity

-- | @since 0.1.0.0
instance {-# OVERLAPS #-} Show a => Show (Field nm a) where
  show (Field nm x) = "Field " ++ show nm ++ " " ++ show x

-- | Bidirectional pattern synonym for 'FieldT' over 'Identity'.
--
-- @
-- 'Field' name value = 'FieldT' name ('Identity' value)
-- @
--
-- @since 0.1.0.0
pattern Field :: Name nm -> a -> Field nm a
pattern Field nm x <-
  FieldT nm (Identity x)
  where
    Field nm x = FieldT nm (Identity x)

{-# COMPLETE Field #-}

-- -----------------------------------------------------------------------------

-- | 'RecT' is an extensible record whose type row is transformed over @m@.
--
-- * @m@ - a type @'Type' -> 'Type'@ transforming types in the record.
--
-- * @row@ - is a row of types @(nm '#' a)@ corresponding to fields
-- @'FieldT' nm m a@.
--
-- @since 0.1.0.0
infixr 5 :|

type RecT :: (Type -> Type) -> [Ascribe Symbol Type] -> Type
data RecT m row where
  RNil :: RecT m '[]
  (:|) :: FieldT nm m a -> RecT m row -> RecT m (nm # a ': row)

-- | @since 0.1.0.0
instance Show (RecT m '[]) where
  show RNil = "RNil"

-- | @since 0.1.0.0
instance (Show (m a), Show (RecT m row)) => Show (RecT m (nm # a ': row)) where
  show (field :| record) = show field ++ " :| " ++ show record

-- | @since 0.1.0.0
instance Eq (RecT m '[]) where
  -- Nominal equality
  RNil == RNil = True

-- | @since 0.1.0.0
instance (Eq (m a), Eq (RecT m row)) => Eq (RecT m (nm # a ': row)) where
  f1 :| r1 == f2 :| r2 = f1 == f2 && r1 == r2
  {-# INLINE (==) #-}

-- | Apple some function @f a -> g a@ "over" the values of the fields. Whether
-- it be a natural transformation or transforming a GADT.
--
-- @since 0.1.0.0
overFields ::
  (forall nm a. FieldT nm f a -> FieldT nm g a) ->
  RecT f cxt ->
  RecT g cxt
overFields _ RNil = RNil
overFields f (field :| record) = f field :| overFields f record
{-# INLINE overFields #-}

infix 5 <:
type (<:) :: Ascribe Symbol Type -> [Ascribe Symbol Type] -> Constraint
class l <: row where
  -- | RecTord projection given the field to project.
  --
  -- @since 0.1.0.0
  getRecT :: l ~ (nm # a) => RecT m row -> Name nm -> m a

  -- | Setting the value of a record's field.
  --
  -- @since 0.1.0.0
  putRecT :: l ~ (nm # a) => RecT m row -> Name nm -> m a -> RecT m row

-- | @since 0.1.0.0
instance {-# OVERLAPS #-} (nm # a) <: ((nm # a) ': row) where
  getRecT (FieldT _ x :| _) _ = x
  {-# INLINE CONLIKE getRecT #-}

  putRecT (_ :| r) nm x = FieldT nm x :| r
  {-# INLINE CONLIKE putRecT #-}

-- | @since 0.1.0.0
instance (nm # a) <: row => (nm # a) <: ((nm' # b) ': row) where
  getRecT (_ :| r) = getRecT r
  {-# INLINE CONLIKE getRecT #-}

  putRecT (field' :| r) nm x = field' :| putRecT r nm x
  {-# INLINE CONLIKE putRecT #-}

-- -----------------------------------------------------------------------------

-- | The type of record fields for 'RecT', indexed by the fields name,
-- transformer, and type.
--
-- @since 0.1.0.0
data FieldT nm m a
  = FieldT {-# UNPACK #-} !(Name nm) (m a)
  deriving (Eq, Functor)

-- | @since 0.1.0.0
instance Show (m a) => Show (FieldT nm m a) where
  show (FieldT nm x) = "FieldT " ++ show nm ++ " " ++ show x
