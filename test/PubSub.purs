module Test.PubSub where

import Cache (SimpleConn, duplicateConn, publish, setMessageHandler, subscribe)
import Data.Array.NonEmpty (singleton)
import Data.Either (Either(..), fromRight)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, pure, show, unit, ($))
import Test.Internal (checkValue)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

testChannel :: String
testChannel = "test-channel"

pubsubTest :: SimpleConn -> Spec Unit
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
        
        liftEffect $ setMessageHandler subConn $ \channel message -> do
          channel `shouldEqual` testChannel
          message `shouldEqual` "bar"

        v1 <- subscribe subConn $ singleton testChannel
        checkValue v1 unit

        v2 <- publish cacheConn testChannel "bar"
        checkValue v2 1

        -- Wait for the message handler to get the message
        delay $ Milliseconds 200.0

        pure unit
