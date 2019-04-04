module Test.Stream where

import Cache (CacheConn, delKey)
import Cache.Stream (Entry(..), EntryID(..), TrimStrategy(..), firstEntryId, xadd, xdel, xgroupCreate, xgroupDelConsumer, xgroupDestroy, xgroupSetId, xlen, xrange, xread, xreadGroup, xrevrange, xtrim)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (length, singleton, (!!))
import Data.Either (Either(..), fromRight)
import Data.Maybe (Maybe(..), fromJust)
import Data.StrMap (lookup, size)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, pure, show, unit, ($), (<>), (==))
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

testStream :: String
testStream = "test-stream"

testGroup :: String
testGroup = "test-group"

testConsumer :: String
testConsumer = "test-consumer"

streamTest :: CacheConn -> Aff _ Unit
streamTest cacheConn = liftEff $ run [consoleReporter] do
  describe "Stream" do
     -- FIXME: break up at commented "it"s on newer purescript-spec which can
     -- force sequential running of tests.
     it "works" do

     --it "returns 0 on non-existent stream" do
        _   <- delKey cacheConn testStream
        len <- xlen cacheConn testStream
        case len of
             Right 0  -> pure unit
             Right v  -> fail $ "Bad value: " <> show v
             Left err -> fail $ "Bad value: " <> show err

     --it "can add a key/value to a stream" do
        id <- xadd cacheConn testStream AutoID $ singleton $ "test" /\ "123"
        case id of
             Right v  -> pure unit
             Left err -> fail $ "Add failed: " <> show err

     --it "can delete a key/value" do
        n <- xdel cacheConn testStream $ unsafePartial $ fromRight id
        case n of
             Right 1  -> pure unit
             Right v  -> fail $ "Deleted more than 1 entry: " <> (show v)
             Left err -> fail $ "Delete failed: " <> show err

     --it "can read from an empty stream"
        val <- xread cacheConn Nothing [Tuple testStream firstEntryId]
        case val of
             Right v  -> size v `shouldEqual` 0
             Left err -> fail $ "Read failed: " <> show err

     --it "can read the values just added" do
        _ <- xadd cacheConn testStream AutoID $ singleton $ "test" /\ "123"
        val <- xread cacheConn Nothing [Tuple testStream firstEntryId]
        case val of
             Right v  -> do
                size v `shouldEqual` 1
                case lookup testStream v of
                     Just entries -> checkEntries entries
                     Nothing -> fail "Could not find stream in result"
             Left err -> fail $ "Read failed: " <> show err

     --it "can read a range of values" do
        val <- xrange cacheConn testStream MinID MaxID Nothing
        case val of
             Right entries -> checkEntries entries
             Left err      -> fail $ "Range failed: " <> show err

     --it "can read a range of values in reverse" do
        val <- xrevrange cacheConn testStream MinID MaxID Nothing
        case val of
             Right entries -> checkEntries entries
             Left err      -> fail $ "Range failed: " <> show err

     --it "returns 1 on first (forced) trim" do
        len <- xtrim cacheConn testStream Maxlen false 0
        case len of
             Right v  -> if v == 1 then pure unit else fail $ "Bad value: " <> show v
             Left err -> fail $ "Bad value: " <> show err

     --it "returns 0 on second (approximate) trim" do
        len <- xtrim cacheConn testStream Maxlen true 0
        case len of
             Right v  -> if v == 0 then pure unit else fail $ "Bad value: " <> show v
             Left err -> fail $ "Bad value: " <> show err

     --it "can create a consumer group" do
        res <- xgroupCreate cacheConn testStream testGroup AfterLastID
        case res of
             Right _  -> pure unit
             Left err -> fail $ "Group create failed: " <> show err

     --it "can create a consumer group only once" do
        res <- xgroupCreate cacheConn testStream testGroup AfterLastID
        case res of
             Right _ -> fail $ "Group create should not have succeeded on duplicate"
             Left _  -> pure unit

     --it "won't find any entries on reading from the group immediately" do
        val <- xreadGroup cacheConn testGroup testConsumer Nothing false [Tuple testStream NewID]
        case val of
             Right v  -> size v `shouldEqual` 0
             Left err -> fail $ "Read from group failed: " <> show err

     --it "can read from a group" do
        _   <- xadd cacheConn testStream AutoID $ singleton $ "test" /\ "123"
        val <- xreadGroup cacheConn testGroup testConsumer Nothing false [Tuple testStream NewID]
        case val of
             Right v  -> do
                size v `shouldEqual` 1
                case lookup testStream v of
                     Just entries -> checkEntries entries
                     Nothing -> fail "Could not find stream in result"
             Left err -> fail $ "Read from group failed: " <> show err

     --it "can delete a consumer in a group" do
        res <- xgroupDelConsumer cacheConn testStream testGroup testConsumer
        case res of
             Right _  -> pure unit
             Left err -> fail $ "Group destroy failed: " <> show err

     --it "can set an ID on a group" do
        res <- xgroupSetId cacheConn testStream testGroup AfterLastID
        case res of
             Right _  -> pure unit
             Left err -> fail $ "Group set ID failed: " <> show err

     --it "can destroy a consumer group" do
        res <- xgroupDestroy cacheConn testStream testGroup
        case res of
             Right _  -> pure unit
             Left err -> fail $ "Group destroy failed: " <> show err

     -- Clean up
        _ <- delKey cacheConn testStream
        pure unit

     where
           checkEntries entries = do
             let (Entry _ items)   = unsafePartial $ fromJust $ entries !! 0
                 (Tuple key value) = unsafePartial $ fromJust $ items !! 0
             length entries `shouldEqual` 1
             length items   `shouldEqual` 1
             key            `shouldEqual` "test"
             value          `shouldEqual` "123"
