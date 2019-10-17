module Test.Basic where

import Cache (CacheConn, SetOptions(..), del, exists, expire, get, incr, incrby, set)
import Cache.Internal (checkValue)
import Data.Array.NonEmpty (singleton, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect.Aff (delay)
import Prelude (Unit, bind, discard, pure, show, unit, ($))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

testKey :: String
testKey = "test-basic"

testKey1 :: String
testKey1 = "test-basic-1"

testKey2 :: String
testKey2 = "test-basic-2"

basicTest :: CacheConn -> Spec Unit
basicTest cacheConn =
  describe "Basic" do
     it "works" do
        _ <- del cacheConn $ testKey : testKey1 : singleton testKey2

        v0 <- set cacheConn testKey "foo" (Just $ Milliseconds 100.0) NoOptions
        case v0 of
             Right _  -> pure unit
             Left err -> fail $ show err

        v4 <- exists cacheConn testKey
        checkValue v4 true

        v1 <- get cacheConn testKey
        checkValue v1 (Just "foo")

        delay $ Milliseconds 200.0

        v2 <- get cacheConn testKey
        checkValue v2 Nothing

        v3 <- exists cacheConn testKey
        checkValue v3 false

        _  <- set cacheConn testKey1 "foo" Nothing NoOptions
        _  <- set cacheConn testKey2 "bar" Nothing NoOptions
        v5 <- del cacheConn $ testKey1 : singleton testKey2
        checkValue v5 2

        _  <- set cacheConn testKey "1" Nothing NoOptions
        v6 <- expire cacheConn testKey (Seconds 1.0)
        checkValue v6 true

        v7 <- incr cacheConn testKey1
        v8 <- incrby cacheConn testKey1 2
        checkValue v7 1
        checkValue v8 3

        v9 <- set cacheConn testKey1 "3" Nothing IfNotExist
        checkValue v9 false

        v10 <- set cacheConn testKey1 "3" Nothing IfExist
        checkValue v10 true

        -- Clean up
        _ <- del cacheConn $ testKey : testKey1 : singleton testKey2
        pure unit