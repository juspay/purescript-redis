module Test.Hash where

import Cache (class CacheConn, del)
import Cache.Hash (hget, hgetall, hincrby, hmget, hmset, hset, hsetnx)
import Data.Array.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (insert, singleton) as Object
import Prelude (Unit, bind, discard, pure, unit, ($))
import Test.Internal (checkValue)
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

        v7 <- hgetall cacheConn testKey
        checkValue v7 (Object.insert "foonx" "1" $ Object.singleton "foo" "1")

        v8 <- hincrby cacheConn testKey "foonx" 2
        checkValue v8 3

        v9 <- hmset cacheConn testKey [Tuple "f1" "1", Tuple "f2" "2"]
        checkValue v9 true

        v10 <- hmget cacheConn testKey ["foo", "f1", "f3"]
        checkValue v10 [ Just "1", Just "1", Nothing ]
        
        -- Clean up
        _  <- del cacheConn $ singleton testKey
        pure unit
