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
import Control.Promise (Promise, toAff)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn5, runFn2, runFn3, runFn4, runFn5, runFn7)
import Data.Int (round)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..), Seconds)
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt, error)
import Foreign (Foreign)
import Prelude (map, pure, show, ($), (<$>), (<<<))

foreign import data Multi :: Type

foreign import newMultiJ :: CacheConn -> Effect Multi

foreign import setMultiJ :: Fn5 String String String String Multi (Effect Multi)
foreign import getMultiJ :: Fn2 String Multi (Effect Multi)
foreign import delMultiJ :: Fn2 (Array String) Multi (Effect Multi)
foreign import expireMultiJ :: Fn3 String Int Multi (Effect Multi)
foreign import incrMultiJ :: Fn2 String Multi (Effect Multi)
foreign import hsetMultiJ :: Fn4 String String String Multi (Effect Multi)
foreign import hgetMultiJ :: Fn3 String String Multi (Effect Multi)
foreign import publishMultiJ :: Fn3 String String Multi (Effect Multi)
foreign import subscribeMultiJ :: Fn2 String Multi (Effect Multi)
foreign import rpopMultiJ :: Fn2 String Multi (Effect Multi)
foreign import rpushMultiJ :: Fn3 String String Multi (Effect Multi)
foreign import lpopMultiJ :: Fn2 String Multi (Effect Multi)
foreign import lpushMultiJ :: Fn3 String String Multi (Effect Multi)
foreign import lindexMultiJ :: Fn3 String Int Multi (Effect Multi)
foreign import execMultiJ :: Multi -> Promise (Array Foreign)

newMulti :: CacheConn -> Effect Multi
newMulti = newMultiJ

execMulti :: Multi -> Aff (Either Error (Array Foreign))
execMulti = attempt <<< toAff <<< execMultiJ

setMulti :: String -> String -> Maybe Milliseconds -> SetOptions -> Multi -> Effect Multi
setMulti key value mExp opts = runFn5 setMultiJ key value (maybe "" msToString mExp) (show opts)
  where
        msToString (Milliseconds v) = show $ round v

getMulti :: String -> Multi -> Effect Multi
getMulti val = runFn2 getMultiJ val

delMulti :: NonEmptyArray String -> Multi -> Effect Multi
delMulti keys = runFn2 delMultiJ (toArray keys)

expireMulti :: String -> Seconds -> Multi -> Effect Multi
expireMulti key ttl = runFn3 expireMultiJ key (round <<< unwrap $ ttl)

incrMulti :: String -> Multi -> Effect Multi
incrMulti key = runFn2 incrMultiJ key

hsetMulti :: String -> String -> String -> Multi -> Effect Multi
hsetMulti key field val = runFn4 hsetMultiJ key field val

hgetMulti :: String -> String -> Multi -> Effect Multi
hgetMulti key field = runFn3 hgetMultiJ key field

publishMulti :: String -> String -> Multi -> Effect Multi
publishMulti channel message = runFn3 publishMultiJ channel message

subscribeMulti :: String -> Multi -> Effect Multi
subscribeMulti channel = runFn2 subscribeMultiJ channel

rpopMulti :: String -> Multi -> Effect Multi
rpopMulti listName = runFn2 rpopMultiJ listName

rpushMulti :: String -> String -> Multi -> Effect Multi
rpushMulti listName value = runFn3 rpushMultiJ listName value

lpopMulti :: String -> Multi -> Effect Multi
lpopMulti listName = runFn2 lpopMultiJ listName

lpushMulti :: String -> String -> Multi -> Effect Multi
lpushMulti listName value = runFn3 lpushMultiJ listName value

lindexMulti :: String -> Int -> Multi -> Effect Multi
lindexMulti listName index = runFn3 lindexMultiJ listName index

xaddMulti :: String -> EntryID -> Array Item -> Multi -> Effect (Either Error Multi)
xaddMulti _ AfterLastID _ _ = pure $ Left $ error "XADD must take a concrete entry ID or AutoID"
xaddMulti _ MinID       _ _ = pure $ Left $ error "XADD must take a concrete entry ID or AutoID"
xaddMulti _ MaxID       _ _ = pure $ Left $ error "XADD must take a concrete entry ID or AutoID"
xaddMulti _ NewID       _ _ = pure $ Left $ error "XADD must take a concrete entry ID or AutoID"
xaddMulti key entryId args multi = map Right $ runFn4 xaddJ multi key (show entryId) $ itemsToArray args

xdelMulti :: String -> EntryID -> Multi -> Effect Multi
xdelMulti key entryId multi = runFn3 xdelJ multi key (show entryId)

xlenMulti :: String -> Multi -> Effect Multi
xlenMulti key multi = runFn2 xlenJ multi key

xrangeMulti :: String -> EntryID -> EntryID -> Maybe Int -> Multi -> Effect Multi
xrangeMulti stream start end mCount multi = runFn5 xrangeJ multi stream (show start) (show end) (fromMaybe 0 mCount)

xrevrangeMulti :: String -> EntryID -> EntryID -> Maybe Int -> Multi -> Effect Multi
xrevrangeMulti stream start end mCount multi = runFn5 xrangeJ multi stream (show start) (show end) (fromMaybe 0 mCount)

xreadMulti :: Maybe Int -> Array (Tuple String EntryID) -> Multi -> Effect Multi
xreadMulti mCount streamIds multi = do
  let count   = fromMaybe 0 mCount
      streams = fst <$> streamIds
      ids     = show <<< snd <$> streamIds
  runFn4 xreadJ multi count streams ids

xtrimMulti :: String -> TrimStrategy -> Boolean -> Int -> Multi -> Effect Multi
xtrimMulti key strategy approx len multi = runFn5 xtrimJ multi key (show strategy) approx len

xgroupCreateMulti :: String -> String -> EntryID -> Multi -> Effect (Either Error Multi)
xgroupCreateMulti _ _ AutoID _ = pure $ Left $ error "XCREATE must take a concrete ID or AfterLastID"
xgroupCreateMulti _ _ MinID  _ = pure $ Left $ error "XCREATE must take a concrete ID or AfterLastID"
xgroupCreateMulti _ _ MaxID  _ = pure $ Left $ error "XCREATE must take a concrete ID or AfterLastID"
xgroupCreateMulti _ _ NewID  _ = pure $ Left $ error "XCREATE must take a concrete ID or AfterLastID"
xgroupCreateMulti key groupName entryId multi = map Right $ runFn4 xgroupCreateJ multi key groupName (show entryId)

xgroupDestroyMulti :: String -> String -> Multi -> Effect Multi
xgroupDestroyMulti key groupName multi = runFn3 xgroupDestroyJ multi key groupName

xgroupDelConsumerMulti :: String -> String -> String -> Multi -> Effect Multi
xgroupDelConsumerMulti key groupName consumerName multi = runFn4 xgroupDelConsumerJ multi key groupName consumerName

xgroupSetIdMulti :: String -> String -> EntryID -> Multi -> Effect (Either Error Multi)
xgroupSetIdMulti _ _ AutoID _ = pure $ Left $ error "XGROUP SETID must take a concrete ID or AfterLastID"
xgroupSetIdMulti _ _ MinID  _ = pure $ Left $ error "XGROUP SETID must take a concrete ID or AfterLastID"
xgroupSetIdMulti _ _ MaxID  _ = pure $ Left $ error "XGROUP SETID must take a concrete ID or AfterLastID"
xgroupSetIdMulti _ _ NewID  _ = pure $ Left $ error "XGROUP SETID must take a concrete ID or AfterLastID"
xgroupSetIdMulti key groupName entryId multi = map Right $ runFn4 xgroupSetIdJ multi key groupName (show entryId)

xreadGroupMulti :: String -> String -> Maybe Int -> Boolean -> Array (Tuple String EntryID) -> Multi -> Effect Multi
xreadGroupMulti groupName consumerName mCount noAck streamIds multi = do
  let count   = fromMaybe 0 mCount
      streams = fst <$> streamIds
      ids     = show <<< snd <$> streamIds
  runFn7 xreadGroupJ multi groupName consumerName count noAck streams ids

xackMulti :: String -> String -> Array EntryID -> Multi -> Effect Multi
xackMulti key group entryIds multi = runFn4 xackJ multi key group (show <$> entryIds)

xclaimMulti :: String -> String -> String -> Int -> Array EntryID -> Boolean -> Multi -> Effect Multi
xclaimMulti key groupName consumerName idleTimeMs entryIds force multi =
  runFn7 xclaimJ multi key groupName consumerName idleTimeMs (show <$> entryIds) force
