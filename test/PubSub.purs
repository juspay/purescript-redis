module Test.PubSub where

import Cache (CacheConn, duplicateConn, publish, setMessageHandler, subscribe)
import Cache.Internal (checkValue)
import Control.Monad.Aff (delay, launchAff_)
import Control.Monad.Eff.Class (liftEff)
import Data.Array.NonEmpty (singleton)
import Data.Either (Either(..), fromRight)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, pure, show, unit, ($))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

testChannel :: String
testChannel = "test-channel"

pubsubTest :: CacheConn -> Spec _ Unit
pubsubTest cacheConn =
  describe "Pub/Sub" do
     it "works" do
        v0 <- publish cacheConn testChannel "foo"
        checkValue v0 0

        eSubConn <- duplicateConn cacheConn Nothing
        case eSubConn of
             Right _  -> pure unit
             Left err -> fail $ show err

        let subConn = unsafePartial $ fromRight eSubConn

        liftEff $ setMessageHandler subConn $ \channel message -> launchAff_ do
           channel `shouldEqual` testChannel
           message `shouldEqual` "bar"

        v1 <- subscribe subConn $ singleton testChannel
        checkValue v1 unit

        v2 <- publish cacheConn testChannel "bar"
        checkValue v2 1

        -- Wait for the message handler to get the message
        delay $ Milliseconds 200.0

        pure unit
