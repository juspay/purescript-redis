module Test.Main where

import Prelude

import Cache as C
import Cache.Cluster as Cluster
import Data.Either (Either(..))
import Data.Options ((:=))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Test.Basic (basicTest)
import Test.Hash (hashTest)
import Test.List (listTest)
import Test.Multi (multiTest)
import Test.PubSub (pubsubTest)
import Test.SortedSet (sortedSetTest)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Stream (streamTest)

startTest :: Aff Unit
startTest = do
    let cacheOpts = C.host := "127.0.0.1" <> C.port := 6379 <> C.db := 0 <> C.socketKeepAlive := true
    eCacheConn <- C.newConn cacheOpts
    eClusterConn <- Cluster.newClusterConn [{ host: "127.0.0.1", port: 7000 }] mempty
    runSpec [consoleReporter] do
       describe "Simple connection"
         case eCacheConn of
           Right cacheConn -> do
              basicTest cacheConn
              listTest cacheConn
              hashTest cacheConn
              multiTest cacheConn
              streamTest cacheConn
              pubsubTest cacheConn
              sortedSetTest cacheConn
           Left err        -> do
              it "fails" do
                 fail $ show err

       describe "Cluster connection"
         case eClusterConn of
           Right cacheConn -> do
              basicTest cacheConn
              listTest cacheConn
              hashTest cacheConn
              multiTest cacheConn
              streamTest cacheConn
              sortedSetTest cacheConn
           Left err        -> do
              it "fails" do
                 fail $ show err

main :: Effect Unit
main = launchAff startTest *> pure unit
