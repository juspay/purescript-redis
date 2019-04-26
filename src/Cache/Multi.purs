module Cache.Multi
  ( Multi
  , delMulti
  , execMulti
  , expireMulti
  , getMulti
  , hgetMulti
  , hsetMulti
  , incrMulti
  , lindexMulti
  , lpopMulti
  , lpushMulti
  , newMulti
  , publishMulti
  , rpopMulti
  , rpushMulti
  , setMulti
  , subscribeMulti
  , xaddMulti
  , xdelMulti
  , xlenMulti
  , xrangeMulti
  , xreadMulti
  , xrevrangeMulti
  , xtrimMulti
  ) where

import Cache.Stream.Internal (itemsToArray, xackJ, xaddJ, xclaimJ, xdelJ, xgroupCreateJ, xgroupDelConsumerJ, xgroupDestroyJ, xgroupSetIdJ, xlenJ, xrangeJ, xreadGroupJ, xreadJ, xtrimJ)
import Cache.Types (CacheConn, EntryID(..), Item, SetOptions, TrimStrategy)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Promise (Promise, toAff)
import Data.Array (concat, filter)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Either (Either(..))
import Data.Foreign (Foreign, isNull)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn5, runFn2, runFn3, runFn4, runFn5, runFn7)
import Data.Int (round)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..), Seconds)
import Data.Tuple (Tuple, fst, snd)
import Prelude (map, not, pure, show, ($), (<$>), (<<<))

foreign import data Multi :: Type

foreign import newMultiJ :: forall e. CacheConn -> Eff e Multi

foreign import setMultiJ :: forall e. Fn5 String String String String Multi (Eff e Multi)
foreign import getMultiJ :: forall e. Fn2 String Multi (Eff e Multi)
foreign import delMultiJ :: forall e. Fn2 (Array String) Multi (Eff e Multi)
foreign import expireMultiJ :: forall e. Fn3 String Int Multi (Eff e Multi)
foreign import incrMultiJ :: forall e. Fn2 String Multi (Eff e Multi)
foreign import hsetMultiJ :: forall e. Fn4 String String String Multi (Eff e Multi)
foreign import hgetMultiJ :: forall e. Fn3 String String Multi (Eff e Multi)
foreign import publishMultiJ :: forall e. Fn3 String String Multi (Eff e Multi)
foreign import subscribeMultiJ :: forall e. Fn2 String Multi (Eff e Multi)
foreign import rpopMultiJ :: forall e. Fn2 String Multi (Eff e Multi)
foreign import rpushMultiJ :: forall e. Fn3 String String Multi (Eff e Multi)
foreign import lpopMultiJ :: forall e. Fn2 String Multi (Eff e Multi)
foreign import lpushMultiJ :: forall e. Fn3 String String Multi (Eff e Multi)
foreign import lindexMultiJ :: forall e. Fn3 String Int Multi (Eff e Multi)
foreign import execMultiJ :: Multi -> Promise (Array (Array Foreign))

newMulti :: forall e. CacheConn -> Eff e Multi
newMulti = newMultiJ

-- execMultiJ returns an array of [err, val]. In the Multi case, we expect all
-- 'err's to be null, so the filter below will result in just all the values
-- being returned. If something goes horribly wrong, the expectation is that
-- the 'err' will be non-null but the 'val' will be null, so the length of the
-- array returned is still consistent with the number of commands dispatched in
-- the MULTI. This is all a bit ugly, but since we're forced to work with
-- Foreigns here at the moment, it will have to do.
execMulti :: forall e. Multi -> Aff e (Either Error (Array Foreign))
execMulti = attempt <<< map filterNulls <<< toAff <<< execMultiJ
  where
        filterNulls = filter (not isNull) <<< concat

setMulti :: forall e. String -> String -> Maybe Milliseconds -> SetOptions -> Multi -> Eff e Multi
setMulti key value mExp opts = runFn5 setMultiJ key value (maybe "" msToString mExp) (show opts)
  where
        msToString (Milliseconds v) = show $ round v

getMulti :: forall e. String -> Multi -> Eff e Multi
getMulti val = runFn2 getMultiJ val

delMulti :: forall e. NonEmptyArray String -> Multi -> Eff e Multi
delMulti keys = runFn2 delMultiJ (toArray keys)

expireMulti :: forall e. String -> Seconds -> Multi -> Eff e Multi
expireMulti key ttl = runFn3 expireMultiJ key (round <<< unwrap $ ttl)

incrMulti :: forall e. String -> Multi -> Eff e Multi
incrMulti key = runFn2 incrMultiJ key

hsetMulti :: forall e. String -> String -> String -> Multi -> Eff e Multi
hsetMulti key field val = runFn4 hsetMultiJ key field val

hgetMulti :: forall e. String -> String -> Multi -> Eff e Multi
hgetMulti key field = runFn3 hgetMultiJ key field

publishMulti :: forall e. String -> String -> Multi -> Eff e Multi
publishMulti channel message = runFn3 publishMultiJ channel message

subscribeMulti :: forall e. String -> Multi -> Eff e Multi
subscribeMulti channel = runFn2 subscribeMultiJ channel

rpopMulti :: forall e. String -> Multi -> Eff e Multi
rpopMulti listName = runFn2 rpopMultiJ listName

rpushMulti :: forall e. String -> String -> Multi -> Eff e Multi
rpushMulti listName value = runFn3 rpushMultiJ listName value

lpopMulti :: forall e. String -> Multi -> Eff e Multi
lpopMulti listName = runFn2 lpopMultiJ listName

lpushMulti :: forall e. String -> String -> Multi -> Eff e Multi
lpushMulti listName value = runFn3 lpushMultiJ listName value

lindexMulti :: forall e. String -> Int -> Multi -> Eff e Multi
lindexMulti listName index = runFn3 lindexMultiJ listName index

xaddMulti :: forall e. String -> EntryID -> Array Item -> Multi -> Eff e (Either Error Multi)
xaddMulti _ AfterLastID _ _ = pure $ Left $ error "XADD must take a concrete entry ID or AutoID"
xaddMulti _ MinID       _ _ = pure $ Left $ error "XADD must take a concrete entry ID or AutoID"
xaddMulti _ MaxID       _ _ = pure $ Left $ error "XADD must take a concrete entry ID or AutoID"
xaddMulti _ NewID       _ _ = pure $ Left $ error "XADD must take a concrete entry ID or AutoID"
xaddMulti key entryId args multi = map Right $ runFn4 xaddJ multi key (show entryId) $ itemsToArray args

xdelMulti :: forall e. String -> EntryID -> Multi -> Eff e Multi
xdelMulti key entryId multi = runFn3 xdelJ multi key (show entryId)

xlenMulti :: forall e. String -> Multi -> Eff e Multi
xlenMulti key multi = runFn2 xlenJ multi key

xrangeMulti :: forall e. String -> EntryID -> EntryID -> Maybe Int -> Multi -> Eff e Multi
xrangeMulti stream start end mCount multi = runFn5 xrangeJ multi stream (show start) (show end) (fromMaybe 0 mCount)

xrevrangeMulti :: forall e. String -> EntryID -> EntryID -> Maybe Int -> Multi -> Eff e Multi
xrevrangeMulti stream start end mCount multi = runFn5 xrangeJ multi stream (show start) (show end) (fromMaybe 0 mCount)

xreadMulti :: forall e. Maybe Int -> Array (Tuple String EntryID) -> Multi -> Eff e Multi
xreadMulti mCount streamIds multi = do
  let count   = fromMaybe 0 mCount
      streams = fst <$> streamIds
      ids     = show <<< snd <$> streamIds
  runFn4 xreadJ multi count streams ids

xtrimMulti :: forall e. String -> TrimStrategy -> Boolean -> Int -> Multi -> Eff e Multi
xtrimMulti key strategy approx len multi = runFn5 xtrimJ multi key (show strategy) approx len

xgroupCreateMulti :: forall e. String -> String -> EntryID -> Multi -> Eff e (Either Error Multi)
xgroupCreateMulti _ _ AutoID _ = pure $ Left $ error "XCREATE must take a concrete ID or AfterLastID"
xgroupCreateMulti _ _ MinID  _ = pure $ Left $ error "XCREATE must take a concrete ID or AfterLastID"
xgroupCreateMulti _ _ MaxID  _ = pure $ Left $ error "XCREATE must take a concrete ID or AfterLastID"
xgroupCreateMulti _ _ NewID  _ = pure $ Left $ error "XCREATE must take a concrete ID or AfterLastID"
xgroupCreateMulti key groupName entryId multi = map Right $ runFn4 xgroupCreateJ multi key groupName (show entryId)

xgroupDestroyMulti :: forall e. String -> String -> Multi -> Eff e Multi
xgroupDestroyMulti key groupName multi = runFn3 xgroupDestroyJ multi key groupName

xgroupDelConsumerMulti :: forall e. String -> String -> String -> Multi -> Eff e Multi
xgroupDelConsumerMulti key groupName consumerName multi = runFn4 xgroupDelConsumerJ multi key groupName consumerName

xgroupSetIdMulti :: forall e. String -> String -> EntryID -> Multi -> Eff e (Either Error Multi)
xgroupSetIdMulti _ _ AutoID _ = pure $ Left $ error "XGROUP SETID must take a concrete ID or AfterLastID"
xgroupSetIdMulti _ _ MinID  _ = pure $ Left $ error "XGROUP SETID must take a concrete ID or AfterLastID"
xgroupSetIdMulti _ _ MaxID  _ = pure $ Left $ error "XGROUP SETID must take a concrete ID or AfterLastID"
xgroupSetIdMulti _ _ NewID  _ = pure $ Left $ error "XGROUP SETID must take a concrete ID or AfterLastID"
xgroupSetIdMulti key groupName entryId multi = map Right $ runFn4 xgroupSetIdJ multi key groupName (show entryId)

xreadGroupMulti :: forall e. String -> String -> Maybe Int -> Boolean -> Array (Tuple String EntryID) -> Multi -> Eff e Multi
xreadGroupMulti groupName consumerName mCount noAck streamIds multi = do
  let count   = fromMaybe 0 mCount
      streams = fst <$> streamIds
      ids     = show <<< snd <$> streamIds
  runFn7 xreadGroupJ multi groupName consumerName count noAck streams ids

xackMulti :: forall e. String -> String -> Array EntryID -> Multi -> Eff e Multi
xackMulti key group entryIds multi = runFn4 xackJ multi key group (show <$> entryIds)

xclaimMulti :: forall e. String -> String -> String -> Int -> Array EntryID -> Boolean -> Multi -> Eff e Multi
xclaimMulti key groupName consumerName idleTimeMs entryIds force multi =
  runFn7 xclaimJ multi key groupName consumerName idleTimeMs (show <$> entryIds) force
