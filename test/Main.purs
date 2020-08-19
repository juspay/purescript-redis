module Test.Main where

import Prelude

import Cache as C
import Cache.Cluster as Cluster
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (fromMaybe, maybe)
import Data.Options (assoc, (:=))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Node.Process (lookupEnv)
import Test.Basic (basicTest)
import Test.Eval (evalTest)
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
    host <- (fromMaybe "127.0.0.1") <$> (liftEffect $ lookupEnv "REDIS_HOST")
    port <- (fromMaybe 6379 <<< fromString <<< fromMaybe "6379") <$> (liftEffect $ lookupEnv "REDIS_PORT")
    db <- (fromMaybe 0 <<< fromString <<< fromMaybe "0") <$> (liftEffect $ lookupEnv "REDIS_DB")
    mpassword <- (liftEffect $ lookupEnv "REDIS_PASSWORD")
    let cacheOpts = C.host := host <> C.port := port <> C.db := db <> C.socketKeepAlive := true <> maybe mempty (assoc C.password) mpassword
    eCacheConn <- C.newConn cacheOpts
    eClusterConn <- Cluster.newClusterConn [{ host: host, port: port }] (maybe mempty (assoc Cluster.password) mpassword)
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
              evalTest cacheConn
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
              evalTest cacheConn
           Left err        -> do
              it "fails" do
                 fail $ show err

main :: Effect Unit
main = launchAff startTest *> pure unit
