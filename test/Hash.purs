module Test.Hash where

import Cache (class CacheConn, del)
import Cache.Hash (hget, hset, hsetnx)
import Cache.Internal (checkValue)
import Data.Array.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Prelude (Unit, bind, discard, pure, unit, ($))
import Test.Spec (Spec, describe, it)

testKey :: String
testKey = "test-hash"

hashTest :: forall a. CacheConn a => a -> Spec Unit
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

        v4 <- hsetnx cacheConn testKey "foonx" "1"
        v5 <- hsetnx cacheConn testKey "foonx" "2"
        v6 <- hget cacheConn testKey "foonx"

        checkValue v4 true
        checkValue v5 false
        checkValue v6 (Just "1")

        -- Clean up
        _  <- del cacheConn $ singleton testKey
        pure unit
