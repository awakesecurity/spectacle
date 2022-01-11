module Control.Comonad.Tape
  ( -- * Tape Comonad
    Tape (Tape, before, focus, after),

    -- ** Construction
    viewl,
    viewr,
    viewAt,

    -- ** Destruction
    toSeq,

    -- ** Operations
    shiftl,
    shiftr,
    tabulater,
    tabulatel,
  )
where

import Control.Comonad (Comonad, duplicate, extend, extract)
import Control.Comonad.Store (ComonadStore, peek, pos, seek)
import Data.Sequence (Seq (Empty, (:<|), (:|>)), (<|), (|>))
import qualified Data.Sequence as Seq

-- ---------------------------------------------------------------------------------------------------------------------

data Tape a = Tape {before :: Seq a, focus :: a, after :: Seq a}
  deriving (Eq, Functor, Show)

-- | @since 0.1.0.0
instance Foldable Tape where
  foldr cons nil = foldr cons nil . toSeq
  {-# INLINE foldr #-}

-- | @since 0.1.0.0
instance Traversable Tape where
  traverse f (Tape lower x upper) = Tape <$> traverse f lower <*> f x <*> traverse f upper
  {-# INLINE traverse #-}

-- | @since 0.1.0.0
instance Comonad Tape where
  extract (Tape _ x _) = x
  {-# INLINE extract #-}

  duplicate tp = Tape (tabulatel tp) tp (tabulater tp)
  {-# INLINE duplicate #-}

  extend f tp = case duplicate tp of
    Tape ls x us -> Tape (f <$> ls) (f x) (f <$> us)

-- | @since 0.1.0.0
instance ComonadStore Int Tape where
  pos (Tape lw _ _) = length lw
  {-# INLINE pos #-}

  peek n
    | n < 0 = extract . shiftl n
    | n > 0 = extract . shiftr n
    | otherwise = extract
  {-# INLINE peek #-}

  seek n
    | n < 0 = shiftl n
    | n > 0 = shiftr n
    | otherwise = id
  {-# INLINE seek #-}

-- ---------------------------------------------------------------------------------------------------------------------

-- | /O(1)/, @'viewl' xs@ constructs a 'Tape' by viewing @xs@ from the left, if it is nonempty.
--
-- @since 0.1.0.0
viewl :: Seq a -> Maybe (Tape a)
viewl Empty = Nothing
viewl (x :<| upper) = Just (Tape mempty x upper)

-- | /O(1)/, @'viewl' xs@ constructs a 'Tape' by viewing @xs@ from the left, if it is nonempty.
--
-- @since 0.1.0.0
viewr :: Seq a -> Maybe (Tape a)
viewr Empty = Nothing
viewr (lower :|> x) = Just (Tape lower x mempty)

-- | /O(log n)/, @'viewAt' i xs@ constructs a 'Tape' focusing the ith element of @xs@, if it is nonempty. @i@ is
-- clamped to the interval [0, i).
--
-- @since 0.1.0.0
viewAt :: Int -> Seq a -> Maybe (Tape a)
viewAt i xs =
  case Seq.splitAt i xs of
    (Empty, Empty) -> Nothing
    (lower :|> x, Empty) -> Just (Tape lower x Empty)
    (lower, x :<| upper) -> Just (Tape lower x upper)

toSeq :: Tape a -> Seq a
toSeq (Tape lower x upper) = lower <> (x <| upper)

shiftl :: Int -> Tape a -> Tape a
shiftl i (Tape lw0 x0 up0) =
  case Seq.splitAt (length lw0 - abs i) lw0 of
    (_, Empty) -> Tape lw0 x0 up0
    (lw, x :<| up) -> Tape lw x (up <> (x0 <| up0))

shiftr :: Int -> Tape a -> Tape a
shiftr i (Tape lw0 x0 up0) =
  case Seq.splitAt (abs i) up0 of
    (Empty, _) -> Tape lw0 x0 up0
    (lw :|> x, up) -> Tape (lw0 <> (x0 <| lw)) x up

tabulatel :: Tape a -> Seq (Tape a)
tabulatel sp@(Tape lower _ _)
  | Seq.null lower = Seq.empty
  | otherwise = tabulatel (shiftl 1 sp) |> shiftl 1 sp

tabulater :: Tape a -> Seq (Tape a)
tabulater sp@(Tape _ _ up)
  | Seq.null up = Seq.empty
  | otherwise = shiftr 1 sp <| tabulater (shiftr 1 sp)
