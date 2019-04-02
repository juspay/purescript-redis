module Test.Stream where

import Cache (CacheConn, EntryID(..), xadd, xlen)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Prelude (Unit, bind, discard, pure, show, unit, ($), (<>))
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail)
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
