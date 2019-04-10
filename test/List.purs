module Test.List where

import Prelude

import Cache (CacheConn, del)
import Cache (lindex, lpop, rpush) as C
import Cache.Internal (checkValue)
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

        v0 <- C.lpop cacheConn testKey
        checkValue v0 Nothing

        v1 <- C.rpush cacheConn testKey "hi"
        checkValue v1 1

        v2 <- C.lpop cacheConn testKey
        checkValue v2 (Just "hi")

        v3 <- C.lpop cacheConn testKey
        checkValue v3 Nothing

        l <- C.rpush cacheConn "DBACTIONS" "SELCT * FROM CUSTOMERS;"
        pop <- C.lpop cacheConn "DBACTIONS"
        peek <- C.lindex cacheConn "DBACTIONS" 0
        checkValue peek Nothing

        -- Clean up
        _ <- del cacheConn $ singleton testKey
        pure unit
