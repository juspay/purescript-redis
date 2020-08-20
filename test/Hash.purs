module Test.Hash where

import Cache (class CacheConn, del)
import Cache.Hash (hdel, hget, hgetall, hincrby, hmget, hmset, hset, hsetnx)
import Data.Array.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Foreign.Object (insert, singleton) as Object
import Prelude (Unit, bind, discard, pure, unit, void, ($))
import Test.Internal (checkValue)
import Test.Spec (Spec, describe, it)

testKey :: String
testKey = "test-hash"

hashTest :: forall a. CacheConn a => a -> Spec Unit
hashTest cacheConn =
  describe "Hash" do
     it "HSET " do
        _  <- del cacheConn $ singleton testKey
        v0 <- hset cacheConn testKey "foo" "1"
        checkValue v0 true
        pure unit
     it "HGET" do
        _  <- del cacheConn $ singleton testKey
        _ <- hset cacheConn testKey "foo" "1"
        v1 <- hget cacheConn testKey "foo"
        checkValue v1 (Just "1")
        v2 <- hget cacheConn "does-not-exist" "foo"
        checkValue v2 Nothing
        v3 <- hget cacheConn testKey "bar"
        checkValue v3 Nothing
     it "HSETNX" do
       _  <- del cacheConn $ singleton testKey
       v4 <- hsetnx cacheConn testKey "foonx" "1"
       checkValue v4 true
       v5 <- hsetnx cacheConn testKey "foonx" "2"
       checkValue v5 false
       v6 <- hget cacheConn testKey "foonx"
       checkValue v6 (Just "1")
     it "HGETALL" do
       _  <- del cacheConn $ singleton testKey
       _ <- hset cacheConn testKey "foo" "1"
       _ <- hsetnx cacheConn testKey "foonx" "1"
       v7 <- hgetall cacheConn testKey
       checkValue v7 (Object.insert "foonx" "1" $ Object.singleton "foo" "1")
       v8 <- hincrby cacheConn testKey "foonx" 2
       checkValue v8 3

       v9 <- hmset cacheConn testKey [Tuple "f1" "1", Tuple "f2" "2"]
       checkValue v9 true
       v10 <- hmget cacheConn testKey ["foo", "f1", "f3"]
       checkValue v10 [ Just "1", Just "1", Nothing ]
     it "HDEL" do
       -- Clean Up before test
       void $ del cacheConn $ singleton testKey
       void $ hset cacheConn testKey "foo" "1"
       v0 <- hdel cacheConn testKey "foo"
       checkValue v0 true
       v1 <- hdel cacheConn testKey "not-exist"
       v2 <- hdel cacheConn "not-exist" "not-exist"
       checkValue v2 false
       -- Clean up
       void $ del cacheConn $ singleton testKey
