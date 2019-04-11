module Test.List where

import Prelude

import Cache (CacheConn, del)
import Cache.Internal (checkValue)
import Cache.List (lindex, lpop, lpush, rpop, rpush)
import Data.Array.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)

testKey :: String
testKey = "test-list"

listTest :: CacheConn -> Spec _ Unit
listTest cacheConn =
  describe "List" do
     it "works" do
        _ <- del cacheConn $ singleton testKey

        v0 <- lpop cacheConn testKey
        checkValue v0 Nothing

        v1 <- rpush cacheConn testKey "hi"
        checkValue v1 1

        v2 <- lpop cacheConn testKey
        checkValue v2 (Just "hi")

        v3 <- lpop cacheConn testKey
        checkValue v3 Nothing

        l <- lpush cacheConn "DBACTIONS" "SELCT * FROM CUSTOMERS;"
        pop <- rpop cacheConn "DBACTIONS"
        peek <- lindex cacheConn "DBACTIONS" 0
        checkValue peek Nothing

        -- Clean up
        _ <- del cacheConn $ singleton testKey
        pure unit
