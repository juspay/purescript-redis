module Test.Basic where

import Cache (CacheConn, SetOptions(..), del, exists, expire, get, incr, incrby, set)
import Cache.Internal (checkValue)
import Control.Monad.Aff (delay)
import Data.Array.NonEmpty (singleton, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Prelude (Unit, bind, discard, pure, show, unit, ($))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

basicTest :: CacheConn -> Spec _ Unit
basicTest cacheConn =
  describe "Basic" do
     it "works" do
        v0 <- set cacheConn "test-key" "foo" (Just $ Milliseconds 100.0) NoOptions
        case v0 of
             Right _  -> pure unit
             Left err -> fail $ show err

        v4 <- exists cacheConn "test-key"
        checkValue v4 true

        v1 <- get cacheConn "test-key"
        checkValue v1 (Just "foo")

        delay $ Milliseconds 200.0

        v2 <- get cacheConn "test-key"
        checkValue v2 Nothing

        v3 <- exists cacheConn "test-key"
        checkValue v3 false

        _  <- set cacheConn "test-key-1" "foo" Nothing NoOptions
        _  <- set cacheConn "test-key-2" "bar" Nothing NoOptions
        v5 <- del cacheConn $ "test-key-1" : singleton "test-key-2"
        checkValue v5 2

        _  <- set cacheConn "test-key" "1" Nothing NoOptions
        v6 <- expire cacheConn "test-key" (Seconds 1.0)
        checkValue v6 true

        v7 <- incr cacheConn "test-key-1"
        v8 <- incrby cacheConn "test-key-1" 2
        checkValue v7 1
        checkValue v8 3
