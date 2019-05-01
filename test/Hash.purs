module Test.Hash where

import Cache (class CacheConn, del)
import Cache.Hash (hget, hset)
import Cache.Internal (checkValue)
import Data.Array.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Prelude (Unit, bind, discard, pure, unit, ($))
import Test.Spec (Spec, describe, it)

testKey :: String
testKey = "test-hash"

hashTest :: forall a. CacheConn a => a -> Spec _ Unit
hashTest cacheConn =
  describe "Hash" do
     it "works" do
        _  <- del cacheConn $ singleton testKey

        v0 <- hset cacheConn testKey "foo" "1"
        v1 <- hget cacheConn testKey "foo"

        checkValue v0 true
        checkValue v1 (Just "1")

        v2 <- hget cacheConn "does-not-exist" "foo"
        checkValue v2 Nothing
        v3 <- hget cacheConn testKey "bar"
        checkValue v3 Nothing

        -- Clean up
        _  <- del cacheConn $ singleton testKey
        pure unit
