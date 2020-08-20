module Test.Main where

import Prelude

import Cache as C
import Cache.Cluster as Cluster
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (fromMaybe, maybe)
import Data.Options (assoc, (:=))
import Data.String (toLower)
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
    chost <- (fromMaybe "127.0.0.1") <$> (liftEffect $ lookupEnv "REDIS_CLUSTER_HOST")
    cport <- (fromMaybe 7000 <<< fromString <<< fromMaybe "7000") <$> (liftEffect $ lookupEnv "REDIS_CLUSTER_PORT")
    cmpassword <- (liftEffect $ lookupEnv "REDIS_CLUSTER_PASSWORD")
    eClusterConn <- Cluster.newClusterConn [{ host: chost, port: cport }] (maybe mempty (assoc Cluster.password) cmpassword)
    skipClusterTest <- (toBoolean <<<fromMaybe "false") <$> (liftEffect $ lookupEnv "SKIP_CLUSTER_TEST")
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
         if skipClusterTest then
           pure unit

         else case eClusterConn of
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


toBoolean :: String -> Boolean
toBoolean s = case toLower s of
  "true" -> true
  _ -> false
