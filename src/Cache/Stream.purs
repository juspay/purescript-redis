module Cache.Stream
  ( firstEntryId
  , newEntryId
  , xack
  , xadd
  , xclaim
  , xdel
  , xgroupCreate
  , xgroupDelConsumer
  , xgroupDestroy
  , xgroupSetId
  , xlen
  , xrange
  , xread
  , xreadGroup
  , xrevrange
  , xtrim
  ) where

import Cache.Stream.Internal

import Cache.Types (class CacheConn, CacheAff, Entry(..), EntryID(..), Item, TrimStrategy)
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Except (Except, runExcept)
import Control.MonadPlus ((>>=))
import Control.Promise (toAff)
import Data.Array (filter, length, range, zip, (!!))
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, fromString, shl) as BigInt
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, isNull, readArray, readString)
import Data.Function.Uncurried (runFn2, runFn3, runFn4, runFn5, runFn7)
import Data.Int (even, odd)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.StrMap (StrMap, empty, fromFoldable)
import Data.String (Pattern(..))
import Data.String.CodePoints (split)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)
import Prelude (class Show, Unit, bind, map, pure, show, ($), (&&), (<), (<$>), (<*>), (<<<), (<>), (==), (>=), (>>>))

-- Streams API

bigZero :: BigInt
bigZero = BigInt.fromInt 0

-- Validate a BigInt as being an unsigned 64-bit integer
filter64bit :: BigInt -> Maybe BigInt
filter64bit n = if n >= bigZero && n < uint64Max then Just n else Nothing
  where
        uint64Max     = BigInt.shl (BigInt.fromInt 1) 64.0

newEntryId :: BigInt -> BigInt -> Maybe EntryID
newEntryId ms seq = EntryID <$> filter64bit ms <*> filter64bit seq

firstEntryId :: EntryID
firstEntryId = EntryID bigZero bigZero

fromString :: String -> Maybe EntryID
fromString s =
  let parts         = split (Pattern "-") s
      ms            = parts !! 0 >>= BigInt.fromString >>= filter64bit
      seq           = parts !! 1 >>= BigInt.fromString >>= filter64bit
  in
    if length parts == 2
      then EntryID <$> ms <*> seq
      else Nothing

xadd :: forall a e. CacheConn a => a -> String -> EntryID -> Array Item -> CacheAff e (Either Error EntryID)
xadd _ _ AfterLastID _ = pure $ Left $ error "XADD must take a concrete entry ID or AutoID"
xadd _ _ MinID       _ = pure $ Left $ error "XADD must take a concrete entry ID or AutoID"
xadd _ _ MaxID       _ = pure $ Left $ error "XADD must take a concrete entry ID or AutoID"
xadd _ _ NewID       _ = pure $ Left $ error "XADD must take a concrete entry ID or AutoID"
xadd cacheConn key entryId args = do
  res <- attempt <<< toAff $ runFn4 xaddJ cacheConn key (show entryId) $ itemsToArray args
  pure $ forceFromString  <$> res

xdel :: forall a e. CacheConn a => a -> String -> EntryID -> CacheAff e (Either Error Int)
xdel cacheConn key entryId = attempt <<< toAff $ runFn3 xdelJ cacheConn key (show entryId)

-- FIXME: this and all Ints should actually be 64-bit (instead of the current 32-bit)
-- Note that the API does not distinguish between a non-existing key and an
-- empty stream
xlen :: forall a e. CacheConn a => a -> String -> CacheAff e (Either Error Int)
xlen cacheConn key = attempt <<< toAff $ runFn2 xlenJ cacheConn key

xrange :: forall a e. CacheConn a => a -> String -> EntryID -> EntryID -> Maybe Int -> CacheAff e (Either Error (Array Entry))
xrange cacheConn stream start end mCount = do
  let count = fromMaybe 0 mCount
  res <- attempt <<< toAff $ runFn5 xrangeJ cacheConn stream (show start) (show end) count
  pure $ res >>= readEntries

xrevrange :: forall a e. CacheConn a => a -> String -> EntryID -> EntryID -> Maybe Int -> CacheAff e (Either Error (Array Entry))
xrevrange cacheConn stream start end mCount = do
  let count = fromMaybe 0 mCount
  res <- attempt <<< toAff $ runFn5 xrangeJ cacheConn stream (show start) (show end) count
  pure $ res >>= readEntries

xread :: forall a e. CacheConn a => a -> Maybe Int -> Array (Tuple String EntryID) -> CacheAff e (Either Error (StrMap (Array Entry)))
xread cacheConn mCount streamIds = do
  let count   = fromMaybe 0 mCount
      streams = fst <$> streamIds
      ids     = show <<< snd <$> streamIds
  res <- attempt <<< toAff $ runFn4 xreadJ cacheConn count streams ids
  pure $ do
     -- Either monad
     response <- res
     if isNull response
       then Right empty
       else do
          arrStreams    <- parseWithError $ readArray response
          streamEntries <- sequence $ readStreamEntries <$> arrStreams
          Right $ fromFoldable streamEntries

xtrim :: forall a e. CacheConn a => a -> String -> TrimStrategy -> Boolean -> Int -> CacheAff e (Either Error Int)
xtrim cacheConn key strategy approx len = attempt <<< toAff $ runFn5 xtrimJ cacheConn key (show strategy) approx len

-- Consumer group API

xgroupCreate :: forall a e. CacheConn a => a -> String -> String -> EntryID -> CacheAff e (Either Error Unit)
xgroupCreate _ _ _ AutoID = pure $ Left $ error "XCREATE must take a concrete ID or AfterLastID"
xgroupCreate _ _ _ MinID  = pure $ Left $ error "XCREATE must take a concrete ID or AfterLastID"
xgroupCreate _ _ _ MaxID  = pure $ Left $ error "XCREATE must take a concrete ID or AfterLastID"
xgroupCreate _ _ _ NewID  = pure $ Left $ error "XCREATE must take a concrete ID or AfterLastID"
xgroupCreate cacheConn key groupName entryId = attempt <<< toAff $ runFn4 xgroupCreateJ cacheConn key groupName (show entryId)

xgroupDestroy :: forall a e. CacheConn a => a -> String -> String -> CacheAff e (Either Error Unit)
xgroupDestroy cacheConn key groupName = attempt <<< toAff $ runFn3 xgroupDestroyJ cacheConn key groupName

xgroupDelConsumer :: forall a e. CacheConn a => a -> String -> String -> String -> CacheAff e (Either Error Unit)
xgroupDelConsumer cacheConn key groupName consumerName = attempt <<< toAff $ runFn4 xgroupDelConsumerJ cacheConn key groupName consumerName

xgroupSetId :: forall a e. CacheConn a => a -> String -> String -> EntryID -> CacheAff e (Either Error Unit)
xgroupSetId _ _ _ AutoID = pure $ Left $ error "XGROUP SETID must take a concrete ID or AfterLastID"
xgroupSetId _ _ _ MinID  = pure $ Left $ error "XGROUP SETID must take a concrete ID or AfterLastID"
xgroupSetId _ _ _ MaxID  = pure $ Left $ error "XGROUP SETID must take a concrete ID or AfterLastID"
xgroupSetId _ _ _ NewID  = pure $ Left $ error "XGROUP SETID must take a concrete ID or AfterLastID"
xgroupSetId cacheConn key groupName entryId = attempt <<< toAff $ runFn4 xgroupSetIdJ cacheConn key groupName (show entryId)

xreadGroup :: forall a e. CacheConn a => a -> String -> String -> Maybe Int -> Boolean -> Array (Tuple String EntryID) -> CacheAff e (Either Error (StrMap (Array Entry)))
xreadGroup cacheConn groupName consumerName mCount noAck streamIds = do
  let count   = fromMaybe 0 mCount
      streams = fst <$> streamIds
      ids     = show <<< snd <$> streamIds
  res <- attempt <<< toAff $ runFn7 xreadGroupJ cacheConn groupName consumerName count noAck streams ids
  pure $ do
     -- Either monad
     response <- res
     if isNull response
       then Right empty
       else do
          arrStreams    <- parseWithError $ readArray response
          streamEntries <- sequence $ readStreamEntries <$> arrStreams
          Right $ fromFoldable streamEntries

xack :: forall a e. CacheConn a => a -> String -> String -> Array EntryID -> CacheAff e (Either Error Int)
xack cacheConn key group entryIds = attempt <<< toAff $ runFn4 xackJ cacheConn key group (show <$> entryIds)

xclaim :: forall a e. CacheConn a => a -> String -> String -> String -> Int -> Array EntryID -> Boolean -> CacheAff e (Either Error (Array Entry))
xclaim cacheConn key groupName consumerName idleTimeMs entryIds force = do
  res <- attempt <<< toAff $ runFn7 xclaimJ cacheConn key groupName consumerName idleTimeMs (show <$> entryIds) force
  pure $ res >>= readEntries

-- Utility functions for parsing

-- unsafePartial here as we expect redis to never give us an invalid entry ID
forceFromString :: String -> EntryID
forceFromString s = unsafePartial $ fromJust $ fromString s

parseWithError :: forall e' a. Show e' => Except e' a -> Either Error a
parseWithError = lmap (error <<< show) <<< runExcept

parseArrayItem :: forall a. String -> Array Foreign -> Int -> (Foreign -> F a) -> Either Error a
parseArrayItem name arr ix parse =
  fromMaybe
    (Left $ error $ "Could not read " <> name)
    (parseWithError <<< parse <$> arr !! ix)

readStreamEntries :: Foreign -> Either Error (Tuple String (Array Entry))
readStreamEntries v = do
   vals     <- parseWithError $ readArray v
   stream   <- parseArrayItem "stream key" vals 0 readString
   fEntries <- parseArrayItem "stream entries" vals 1 readArray
   entries  <- readEntries fEntries
   Right $ Tuple stream entries

readEntries :: Array Foreign -> Either Error (Array Entry)
readEntries e = sequence $ readEntry <$> e

readEntry :: Foreign -> Either Error Entry
readEntry v = do
   vals   <- parseWithError $ readArray v
   entry  <- parseArrayItem "entry ID" vals 0 (map forceFromString <<< readString)
   fItems <- parseArrayItem "entry items" vals 1 readArray
   items  <- readItems fItems
   Right $ Entry entry items

readItems :: Array Foreign -> Either Error (Array Item)
readItems v =
  if odd $ length v
    then
      Left $ error "unexpected number of keys + values"
    else do
      array <- sequence $ parseWithError <<< readString <$> v
      let indexed = zip (range 1 $ length v) array
          keys    = snd <$> filter (fst >>> odd) indexed
          values  = snd <$> filter (fst >>> even) indexed
      Right $ zip keys values
