module Test.SortedSet where

import Cache.SortedSet

import Cache (class CacheConn, SetOptions(..), del)
import Data.Array.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (Unit, bind, discard, pure, unit, ($))
import Test.Internal (checkValue)
import Test.Spec (Spec, describe, it)

testKey :: String
testKey = "test-sortedSet"

testId :: String
testId = "test-uniqueId"

testId1 :: String
testId1 = "test-uniqueId-1"

sortedSetTest :: forall a. CacheConn a => a -> Spec Unit
sortedSetTest cacheConn =
  describe "SortedSet" do
     it "works" do
        _  <- del cacheConn $ singleton testKey

        v0 <- zadd cacheConn testKey NoOptions Default [ Tuple 1.0 testId, Tuple 1.0 testId1 ]
        v1 <- zadd cacheConn testKey IfNotExist Default [ Tuple 1.0 testId ]
        v2 <- zadd cacheConn testKey IfExist Changed [ Tuple 2.0 testId, Tuple 3.0 testId1 ]
        v3 <- zrange cacheConn testKey 0 2
        v4 <- zincrby cacheConn testKey 2.0 testId

        checkValue v0 2
        checkValue v1 0
        checkValue v2 2
        checkValue v3 [Just testId, Just testId1]
        checkValue v4 (Just 4.0)

        v5 <- zpopmax cacheConn testKey 1
        checkValue v5 [ Tuple testId "4" ]

        v6 <- zpopmin cacheConn testKey 1
        checkValue v6 [ Tuple testId1 "3" ]

        _ <- zadd cacheConn testKey IfNotExist Changed [ Tuple 2.0 testId, Tuple 3.0 testId1 ]
        v7 <- zrem cacheConn testKey [testId, testId1]
        checkValue v7 2
        -- Clean up
        _  <- del cacheConn $ singleton testKey
        pure unit
