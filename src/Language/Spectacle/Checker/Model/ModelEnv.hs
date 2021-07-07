{-# LANGUAGE RecordWildCards #-}

module Language.Spectacle.Checker.Model.ModelEnv
  ( -- * Model Contexts
    ModelEnv
      ( ModelEnv,
        _modelFairness,
        _modelAction,
        _modelFormula,
        _modelTerminate,
        _modelTrace,
        _modelInitialWorld,
        _livenessPropertyNames,
        _disjunctQueue,
        _srcLocMap
      ),

    -- ** Lenses
    modelFairness,
    modelAction,
    modelFormula,
    modelTerminate,
    modelTrace,
    modelInitialWorld,
    livenessPropertyNames,
    disjunctQueue,
    srcLocMap,

    -- * Model Environment Operations
    makeSrcLocMap,

    -- * Disjunction Zipper
    DisjunctZipper (LeftBranch, RightBranch),

    -- ** Construction
    makeDisjunctZips,
  )
where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import Data.List (nub)
import Data.Sequence (Seq)
import GHC.Stack (SrcLoc)
import Lens.Micro (Lens', SimpleGetter, lens, to)

import Language.Spectacle.AST (Action, Invariant, Terminate)
import Language.Spectacle.Checker.Fairness (Fairness)
import Language.Spectacle.Checker.Fingerprint (Fingerprint)
import Language.Spectacle.Checker.World (World)
import Language.Spectacle.Syntax.Modal.Term
  ( Term
      ( Always,
        Complement,
        Conjunct,
        Disjunct,
        Eventually,
        InfinitelyOften,
        StaysAs,
        UpUntil,
        Value
      ),
  )

-- ---------------------------------------------------------------------------------------------------------------------

-- | The 'ModelEnv' is a collection of constants and branch-dependent state held by the model checker.
--
-- @since 0.1.0.0
data ModelEnv spec = ModelEnv
  { -- | The fairness constraint specified to the model checker.
    _modelFairness :: !Fairness
  , -- | The model's next-state relation.
    _modelAction :: Action spec Bool
  , -- | The model's temporal formula.
    _modelFormula :: Invariant spec Bool
  , -- | An optional termination condition specified to the model.
    _modelTerminate :: Maybe (Terminate spec Bool)
  , -- | An execution trace accumulated while traversing a model's state space.
    _modelTrace :: {-# UNPACK #-} !(Seq Fingerprint)
  , -- | The initial world the model began from.
    _modelInitialWorld :: {-# UNPACK #-} !(World spec)
  , -- | A set of names for eventually-qualified expression in '_modelFormula'.
    _livenessPropertyNames :: {-# UNPACK #-} !IntSet
  , -- | The particular branchs of disjunction in '_modelFormula' this model is obligated to fulfill.
    _disjunctQueue :: {-# UNPACK #-} ![DisjunctZipper]
  , -- | A mapping from the names of temporal expressiongs in '_modelFormula' to their souce locations, if they could be
    -- obtained.
    _srcLocMap :: {-# UNPACK #-} !(IntMap (Maybe SrcLoc))
  }

-- | Lens focusing on the fairness constraint of a 'ModelEnv'.
--
-- @since 0.1.0.0
modelFairness :: SimpleGetter (ModelEnv spec) Fairness
modelFairness = to _modelFairness
{-# INLINE modelFairness #-}

-- | Lens focusing on the next-state relation of a 'ModelEnv'.
--
-- @since 0.1.0.0
modelAction :: SimpleGetter (ModelEnv spec) (Action spec Bool)
modelAction = to _modelAction
{-# INLINE modelAction #-}

-- | Lens focusing on the temporal formula of a 'ModelEnv'.
--
-- @since 0.1.0.0
modelFormula :: Lens' (ModelEnv spec) (Invariant spec Bool)
modelFormula = lens _modelFormula \ModelEnv {..} x -> ModelEnv {_modelFormula = x, ..}
{-# INLINE modelFormula #-}

-- | Lens focusing on the termination condition (if one exists) specified by a model.
--
-- @since 0.1.0.0
modelTerminate :: SimpleGetter (ModelEnv spec) (Maybe (Terminate spec Bool))
modelTerminate = to _modelTerminate
{-# INLINE modelTerminate #-}

-- | Lens focusing on the model's behavior.
--
-- @since 0.1.0.0
modelTrace :: Lens' (ModelEnv spec) (Seq Fingerprint)
modelTrace = lens _modelTrace \ModelEnv {..} x -> ModelEnv {_modelTrace = x, ..}
{-# INLINE modelTrace #-}

-- | Lens focusing on the initial world the model was started from.
--
-- @since 0.1.0.0
modelInitialWorld :: SimpleGetter (ModelEnv spec) (World spec)
modelInitialWorld = to _modelInitialWorld
{-# INLINE modelInitialWorld #-}

-- | Lens focusing on the bound names in this context's temporal formula.
--
-- @since 0.1.0.0
livenessPropertyNames :: SimpleGetter (ModelEnv spec) IntSet
livenessPropertyNames = to _livenessPropertyNames
{-# INLINE livenessPropertyNames #-}

-- | Lens focusing disjunction branch this context is committed to.
--
-- @since 0.1.0.0
disjunctQueue :: Lens' (ModelEnv spec) [DisjunctZipper]
disjunctQueue = lens _disjunctQueue \ModelEnv {..} x -> ModelEnv {_disjunctQueue = x, ..}
{-# INLINE disjunctQueue #-}

-- | Lens focusing on the source location map held by a 'ModelEnv'.
--
-- @since 0.1.0.0
srcLocMap :: SimpleGetter (ModelEnv spec) (IntMap (Maybe SrcLoc))
srcLocMap = to _srcLocMap
{-# INLINE srcLocMap #-}

-- | 'makeSrcLocMap' constructs a mapping from the names of temporal expressions to the their source location.
--
-- @since 0.1.0.0
makeSrcLocMap :: Term Bool -> IntMap (Maybe SrcLoc)
makeSrcLocMap = \case
  Value {} -> IntMap.empty
  Conjunct e1 e2 -> makeSrcLocMap e1 `IntMap.union` makeSrcLocMap e2
  Disjunct e1 e2 -> makeSrcLocMap e1 `IntMap.union` makeSrcLocMap e2
  Complement e -> makeSrcLocMap e
  Always srcLoc name e -> IntMap.insert name srcLoc (makeSrcLocMap e)
  Eventually srcLoc name e -> IntMap.insert name srcLoc (makeSrcLocMap e)
  UpUntil srcLoc name e1 e2 -> IntMap.insert name srcLoc (makeSrcLocMap e1 `IntMap.union` makeSrcLocMap e2)
  StaysAs srcLoc name e -> IntMap.insert name srcLoc (makeSrcLocMap e)
  InfinitelyOften srcLoc name e -> IntMap.insert name srcLoc (makeSrcLocMap e)
{-# INLINE makeSrcLocMap #-}

-- | 'DisjunctZipper' commits a disjunction branch in a 'ModelEnv'. A context with the
-- @'_disjunctQueue' = ['LeftBranch']@ will have a temporal formula with exactly one disjunction @e1 \/ e2@ and only
-- check @e1@ in the disjunction.
--
-- @since 0.1.0.0
data DisjunctZipper = LeftBranch | RightBranch
  deriving (Show, Enum, Eq)

-- | Constructs a set of unique disjunction paths to commit to model contexts. @'makeDisjunctZips' f@ for the formula
-- @f = always (p \/ q)@ will run two seperate models, one checking the 'LeftBranch' @always p@ and the other checking
-- @always q@.
--
-- @since 0.1.0.0
makeDisjunctZips :: Term Bool -> [[DisjunctZipper]]
makeDisjunctZips terms =
  let disjunctQueues = nub . filter (not . null) $ go terms
   in if null disjunctQueues
        then return []
        else disjunctQueues
  where
    go :: Term Bool -> [[DisjunctZipper]]
    go = \case
      Value {} -> return []
      Conjunct e1 e2 -> go e1 <> go e2
      Disjunct e1 e2 -> map (LeftBranch :) (go e1) <> map (RightBranch :) (go e2)
      Complement e -> go e
      Always _ _ e -> go e
      Eventually _ _ e -> go e
      UpUntil _ _ e1 e2 -> go e1 <> go e2
      StaysAs _ _ e -> go e
      InfinitelyOften _ _ e -> go e
{-# INLINE makeDisjunctZips #-}
