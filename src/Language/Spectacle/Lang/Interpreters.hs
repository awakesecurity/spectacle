module Language.Spectacle.Lang.Interpreters
  ( handleRelay,
    handleRelayS,
    replaceRelay,
    replaceRelayS,
    interposeRelay,
  )
where

import Data.FTCQueue (FTCQueue (Leaf))
import Data.Type.Induction (WInductMaybe)
import Language.Spectacle.Lang.Control
  ( Control (Done, Next),
    Frame (Frame),
    Store (Store),
  )
import Language.Spectacle.Lang.Internal
  ( Lang (Lang, unLang),
    queueApp,
    queueCompose,
  )
import Language.Spectacle.Lang.Sum (decompose, project, weaken, type (|>))
import Language.Spectacle.Type.Rec (Rec)

-- -----------------------------------------------------------------------------

-- | Handles all requests for an effect @m@.
--
-- @since 0.1.0.0
handleRelay ::
  (a -> Lang sig cxt b) ->
  (forall x. Rec cxt -> m (Lang sig cxt) x -> (x -> Lang sig cxt b) -> Lang sig cxt b) ->
  Lang (m ': sig) cxt a ->
  Lang sig cxt b
handleRelay onReturn hdl = loop
  where
    loop (Lang m) = Lang \store -> case m store of
      Done (Store store' x) -> unLang (onReturn x) store'
      Next (Frame hdls q) -> case decompose hdls of
        Right x -> unLang (hdl store x k) store
        Left other ->
          Next (Frame other (Leaf \store' x -> unLang (k x) store'))
        where
          k = queueCompose q loop

-- | A parameterized version of 'handleRelay'. This is exactly like
-- 'handleRelay' but threads an extra value of type @s@ through the
-- handler's continuation.
--
-- @since 0.1.0.0
handleRelayS ::
  s ->
  (s -> a -> Lang sig cxt b) ->
  (forall x. Rec cxt -> s -> m (Lang sig cxt) x -> (s -> x -> Lang sig cxt b) -> Lang sig cxt b) ->
  Lang (m ': sig) cxt a ->
  Lang sig cxt b
handleRelayS initState onReturn hdl = loop initState
  where
    loop st (Lang m) = Lang \store -> case m store of
      Done (Store store' x) -> unLang (onReturn st x) store'
      Next (Frame hdls q) -> case decompose hdls of
        Right x -> unLang (hdl store st x k) store
        Left other ->
          Next (Frame other (Leaf \store' x -> unLang (k st x) store'))
        where
          k st' x = loop st' $ queueApp q x

-- | Handles an effect by transforming it into another effect.
--
-- @since 0.1.0.0
replaceRelay ::
  WInductMaybe g sig =>
  (a -> Lang (g ': sig) cxt b) ->
  (forall x. Rec cxt -> f (Lang sig cxt) x -> (x -> Lang (g ': sig) cxt b) -> Lang (g ': sig) cxt b) ->
  Lang (f ': sig) cxt a ->
  Lang (g ': sig) cxt b
replaceRelay onReturn bind = loop
  where
    loop (Lang m) = Lang \store -> case m store of
      Done (Store store' x) -> unLang (onReturn x) store'
      Next (Frame hdls q) -> case decompose hdls of
        Right x -> unLang (bind store x k) store
        Left other ->
          Next (Frame (weaken other) (Leaf \store' x -> unLang (k x) store'))
        where
          k = queueCompose q loop

-- | A parameterized version of 'replaceRelay'. This is exactly like
-- 'replaceRelay' but, 'replaceRelayS' threads a value of type @s@ through the
-- handler's continuation.
--
-- @since 0.1.0.0
replaceRelayS ::
  WInductMaybe g sig =>
  s ->
  (s -> a -> Lang (g ': sig) cxt b) ->
  (forall x. Rec cxt -> s -> f (Lang sig cxt) x -> (s -> x -> Lang (g ': sig) cxt b) -> Lang (g ': sig) cxt b) ->
  Lang (f ': sig) cxt a ->
  Lang (g ': sig) cxt b
replaceRelayS st onReturn bind = loop st
  where
    loop st' (Lang m) = Lang \store -> case m store of
      Done (Store store' x) -> unLang (onReturn st' x) store'
      Next (Frame hdls q) -> case decompose hdls of
        Right x -> unLang (bind store st' x k) store
        Left other ->
          Next (Frame (weaken other) (Leaf \store' x -> unLang (k st' x) store'))
        where
          k st'' x = loop st'' $ queueApp q x

-- | 'interpose' can add intermediate computations to effects in @sig@.
--
-- @since 0.1.0.0
interposeRelay ::
  m |> sig =>
  (a -> Lang sig cxt b) ->
  (forall x. Rec cxt -> m (Lang sig cxt) x -> (x -> Lang sig cxt b) -> Lang sig cxt b) ->
  Lang sig cxt a ->
  Lang sig cxt b
interposeRelay onReturn hdl = loop
  where
    loop (Lang m) = Lang \store -> case m store of
      Done (Store store' x) -> unLang (onReturn x) store'
      Next (Frame hdls q) -> case project hdls of
        Just x -> unLang (hdl store x k) store
        Nothing -> Next (Frame hdls (Leaf \store' x -> unLang (k x) store'))
        where
          k = queueCompose q loop
