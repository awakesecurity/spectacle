{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Language.Spectacle.Interaction
  (
  )
where



-- ---------------------------------------------------------------------------------------------------------------------



-- import Control.Monad (forM_, when)
-- import Control.Monad.Except
--   ( ExceptT,
--     MonadIO (liftIO),
--     runExceptT,
--   )
-- import Data.Function (on)
-- import Data.Hashable (Hashable)
-- import Data.Set (Set)
-- import qualified Data.Set as Set
-- import Data.Text.Prettyprint.Doc (annotate, hardline)
-- import Data.Text.Prettyprint.Doc.Render.Terminal
--   ( Color (Red),
--     bold,
--     color,
--     putDoc,
--   )
-- import Lens.Micro ((&))
-- import Lens.Micro.Mtl (view)
-- import System.Environment (getArgs)
-- import System.Exit (exitFailure, exitSuccess)
-- import System.IO
--   ( BufferMode (LineBuffering),
--     hSetBuffering,
--     stderr,
--     stdout,
--   )
-- import Text.Megaparsec (runParser, (<|>))

-- import Control.Monad.Levels
--   ( LevelsT,
--     forAp,
--     runLevelsA,
--   )
-- import Data.Type.Rec (Rec)
-- import Data.World (World, worldFingerprint)
-- import Language.Spectacle.Checker (modelCheck)
-- import Language.Spectacle.Checker.Fingerprint (Fingerprint)
-- import Language.Spectacle.Checker.MCError (MCError)
-- import Language.Spectacle.Model
-- import Language.Spectacle.Interaction.Render
-- import Language.Spectacle.Specification
--   ( ActionSet (ActionSet, actionSetName, actionSetWorlds),
--     ActionSpine,
--     HasActions (ActionCtxt, takeActionSpine),
--     HasVariables (VariableCtxt),
--     Spec (Spec),
--     Specification,
--     -- specInitialWorlds,
--   )

-- -- ---------------------------------------------------------------------------------------------------------------------

-- -- | 'defaultInteraction' is an 'IO' action which handles rendering model failures or success to terminal. Given some
-- -- specification @spec@
-- --
-- -- @
-- -- main :: IO ()
-- -- main = defaultIntraction (modelCheck spec)
-- -- @
-- --
-- -- is all that is needed to perform model checks and output the results.
-- --
-- -- @since 0.1.0.0
-- defaultInteraction ::
--   forall vars spec prop ctxt acts.
--   ( Specification vars spec prop
--   , VariableCtxt vars ~ ctxt
--   , ActionCtxt ctxt spec ~ acts
--   , ViewableSignature ctxt
--   , Show (Rec ctxt)
--   , Ord (Rec ctxt)
--   , Hashable (Rec ctxt)
--   ) =>
--   Spec vars spec prop ->
--   IO ()
-- defaultInteraction spec = do
--   hSetBuffering stdout LineBuffering
--   hSetBuffering stderr LineBuffering

--   CmdArgs {..} <- getArgsCLI >>= handleErrsCLI

--   case argTrace of
--     Nothing -> do
--       result <- runModelCheck spec
--       print result
--     Just (ReplaySubtreeOpts minFP maxFP) -> undefined
--     Just (ReplayDepthOpts minD maxD) -> undefined
--     Just (ReplayBoundOpts boundD) -> undefined

-- data HoareTriple ctxt = HoareTriple
--   { hoarePrecond :: World ctxt
--   , hoareAction :: String
--   , hoarePostcond :: World ctxt
--   }
--   deriving (Eq, Ord)

-- newtype PostEqTriple ctxt = PostEqTriple
--   {getPostEqTriple :: HoareTriple ctxt}

-- -- | @since 0.1.0.0
-- instance Eq (PostEqTriple ctxt) where
--   PostEqTriple x == PostEqTriple y = hoareAction x == hoareAction y && hoarePostcond x == hoarePostcond y
--   {-# INLINE (==) #-}

-- -- | @since 0.1.0.0
-- instance Ord (PostEqTriple ctxt) where
--   compare x y
--     | x == y = EQ
--     | otherwise = (compare `on` getPostEqTriple) x y

-- emitReplayTrace ::
--   forall vars spec prop ctxt acts.
--   ( Specification vars spec prop
--   , VariableCtxt vars ~ ctxt
--   , ActionCtxt ctxt spec ~ acts
--   , ViewableSignature ctxt
--   , Show (Rec ctxt)
--   , Hashable (Rec ctxt)
--   ) =>
--   Fingerprint ->
--   Fingerprint ->
--   Spec vars spec prop ->
--   Set Behavior ->
--   IO ()
-- emitReplayTrace fpFrom fpTo spec@(Spec _ sp) behavior' = do
--   emit <- undefined
--     -- traceBFS initialWorlds behavior'
--     --   & runLevelsA
--     --   & runExceptT

--   case emit of
--     Left errs -> do
--       undefined
--       -- putDoc =<< renderModelErrorsDoc errs
--     Right _ -> return ()
--   where
--     spine :: ActionSpine ctxt acts
--     spine = takeActionSpine sp

--     initialWorlds :: Set (World ctxt)
--     initialWorlds = undefined
--       -- specInitialWorlds spec

--     traceBFS ::
--       Set (World ctxt) ->
--       Set Behavior ->
--       LevelsT (ExceptT [MCError ctxt] IO) ()
--     traceBFS worldsHere behavior = undefined
--       -- case Set.minView behavior of
--       -- Nothing -> return ()
--       -- Just (Behavior depth actions, xs) -> do
--       --   triples <- forAp worldsHere \worldHere -> do
--       --     nexts <- undefined
--       --       -- modelNextSets worldHere spine

--       --     when (depth == 1) do
--       --       let worldView = newWorldView worldHere
--       --           fpHere = view worldFingerprint worldHere
--       --           worldDoc =
--       --             if fpHere == fpTo || fpHere == fpFrom
--       --               then annotate (bold <> color Red) (ppWorldView 0 "<initial>" fpHere worldView <> hardline)
--       --               else ppWorldView 0 "<initial>" fpHere worldView <> hardline

--       --       liftIO (putDoc worldDoc)

--       --     -- forAp nexts \ActionSet {..} -> do
--       --     --   if actionSetName `elem` actions
--       --     --     then return (Set.map (PostEqTriple . HoareTriple worldHere actionSetName) actionSetWorlds)
--       --     --     else return Set.empty

--       --   liftIO do
--       --     forM_ triples \(PostEqTriple HoareTriple {..}) -> do
--       --       let worldView = newWorldView hoarePostcond
--       --           fpPrecond = view worldFingerprint hoarePrecond
--       --           fpPostcond = view worldFingerprint hoarePostcond

--       --           worldDoc =
--       --             if fpPostcond == fpTo || fpPostcond == fpFrom
--       --               then annotate (bold <> color Red) (ppWorldView depth hoareAction fpPrecond worldView <> hardline)
--       --               else ppWorldView depth hoareAction fpPrecond worldView <> hardline

--       --       putDoc worldDoc

--       --   return () <|> traceBFS (Set.map (hoarePostcond . getPostEqTriple) triples) xs
