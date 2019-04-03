module Test.Stream where

import Cache (CacheConn, delKey)
import Cache.Stream (Entry(..), EntryID(..), TrimStrategy(..), firstEntryId, xadd, xlen, xread, xtrim)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (length, singleton, (!!))
import Data.Either (Either(..))
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

testQueue :: String
testQueue = "test-queue"

streamTest :: CacheConn -> Aff _ Unit
streamTest cacheConn = liftEff $ run [consoleReporter] do
  describe "Stream" do
     -- FIXME: break up at commented "it"s on newer purescript-spec which can
     -- force sequential running of tests.
     it "works" do

     --it "returns 0 on non-existent queue" do
        _   <- delKey cacheConn testQueue
        len <- xlen cacheConn testQueue
        case len of
             Right 0  -> pure unit
             Right v  -> fail $ "Bad value: " <> show v
             Left err -> fail $ "Bad value: " <> show err

     --it "can add a key/value to a queue" do
        id <- xadd cacheConn testQueue AutoID $ singleton $ "test" /\ "123"
        case id of
             Right v  -> pure unit
             Left err -> fail $ "Add failed: " <> show err

     --it "can read the values just added" do
        val <- xread cacheConn Nothing [Tuple testQueue firstEntryId]
        case val of
             Right v  -> do
                size v `shouldEqual` 1
                case lookup testQueue v of
                     Just entries -> do
                        let (Entry _ items)   = unsafePartial $ fromJust $ entries !! 0
                        let (Tuple key value) = unsafePartial $ fromJust $ items !! 0
                        length entries `shouldEqual` 1
                        length items   `shouldEqual` 1
                        key            `shouldEqual` "test"
                        value          `shouldEqual` "123"
                     Nothing -> fail "Could not find queue in result"
             Left err -> fail $ "Read failed: " <> show err

     --it "returns 1 on first (forced) trim" do
        len <- xtrim cacheConn testQueue Maxlen false 0
        case len of
             Right v  -> if v == 1 then pure unit else fail $ "Bad value: " <> show v
             Left err -> fail $ "Bad value: " <> show err

     --it "returns 0 on second (approximate) trim" do
        len <- xtrim cacheConn testQueue Maxlen true 0
        case len of
             Right v  -> if v == 0 then pure unit else fail $ "Bad value: " <> show v
             Left err -> fail $ "Bad value: " <> show err
