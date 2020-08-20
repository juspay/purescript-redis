module Test.Basic where

import Cache (SetOptions(..), del, exists, expire, get, incr, incrby, set)
import Cache.Types (class CacheConn)
import Data.Array.NonEmpty (singleton, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect.Aff (delay)
import Prelude (Unit, bind, discard, pure, show, unit, void, ($))
import Test.Internal (checkValue)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

testKey :: String
testKey = "test-basic{test2}"

testKey1 :: String
testKey1 = "test-basic-1{test2}"

testKey2 :: String
testKey2 = "test-basic-2{test2}"

basicTest :: forall a. CacheConn a => a -> Spec Unit
basicTest cacheConn =
  describe "Basic" do
     it "SET" do
        void $ del cacheConn $ testKey : testKey1 : singleton testKey2
        v0 <- set cacheConn testKey "foo" (Just $ Milliseconds 100.0) NoOptions
        case v0 of
             Right _  -> pure unit
             Left err -> fail $ show err
        void $ del cacheConn $ testKey : testKey1 : singleton testKey2
     it "EXISTS" do
        void $ del cacheConn $ testKey : testKey1 : singleton testKey2
        void $ set cacheConn testKey "foo" (Just $ Milliseconds 100.0) NoOptions
        v4 <- exists cacheConn testKey
        checkValue v4 true
        delay $ Milliseconds 200.0
        v3 <- exists cacheConn testKey
        checkValue v3 false
        void $ del cacheConn $ testKey : testKey1 : singleton testKey2
     it "GET" do
        void $ del cacheConn $ testKey : testKey1 : singleton testKey2
        void $ set cacheConn testKey "foo" (Just $ Milliseconds 100.0) NoOptions
        v1 <- get cacheConn testKey
        checkValue v1 (Just "foo")
        delay $ Milliseconds 200.0
        v2 <- get cacheConn testKey
        checkValue v2 Nothing
        void $ del cacheConn $ testKey : testKey1 : singleton testKey2
     it "DEL" do
        void $ del cacheConn $ testKey : testKey1 : singleton testKey2
        void $ set cacheConn testKey1 "foo" Nothing NoOptions
        void $ set cacheConn testKey2 "bar" Nothing NoOptions
        v5 <- del cacheConn $ testKey1 : singleton testKey2
        checkValue v5 2
        void $ del cacheConn $ testKey : testKey1 : singleton testKey2
     it "EXPIRE" do
        void $ del cacheConn $ testKey : testKey1 : singleton testKey2
        void $ set cacheConn testKey "1" Nothing NoOptions
        v6 <- expire cacheConn testKey (Seconds 1.0)
        checkValue v6 true
        void $ del cacheConn $ testKey : testKey1 : singleton testKey2
     it  "INCR" do
       void $ del cacheConn $ testKey : testKey1 : singleton testKey2
       v7 <- incr cacheConn testKey1
       v8 <- incrby cacheConn testKey1 2
       checkValue v7 1
       checkValue v8 3
       void $ del cacheConn $ testKey : testKey1 : singleton testKey2
