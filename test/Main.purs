module Test.Main where

import Prelude

import Cache (CACHE, db, exec, getConn, getHashKey, getHashKeyMulti, getKey, getKeyMulti, getMulti, host, incr, incrMulti, lpop, port, rpush, setHash, setHashMulti, setKey, setKeyMulti, setMulti, setex, setexKeyMulti, socketKeepAlive) as C
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Data.Options (options, (:=))
import Debug.Trace (spy, traceShow)
import Test.Multi (multiTest)
import Test.Queue (queueTest)
import Test.RedisStreams (redisStreamTest)

foreign import startContext :: forall e a. String -> Eff e a -> Eff e Unit

startTest :: forall e. Aff _ Unit
startTest = do
    let cacheOpts = C.host := "127.0.0.1" <> C.port := 6379 <> C.db := 0 <> C.socketKeepAlive := true
    cacheConn <- C.getConn cacheOpts
    multiTest cacheConn
    queueTest cacheConn
    redisStreamTest cacheConn
    pure unit

main :: forall e. Eff _ Unit
main = startContext "awesome" (launchAff startTest) *> pure unit
