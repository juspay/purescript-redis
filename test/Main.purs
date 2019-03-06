module Test.Main where

import Prelude

import Cache (CACHE, db, lpop, rpush, exec, getConn, getKey, getKeyMulti, getMulti, host, incr, incrMulti, port, setKey, setKeyMulti, setMulti, setex, setexKeyMulti, socketKeepAlive) as C
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Data.Options (options, (:=))
import Debug.Trace (spy, traceShow)

foreign import startContext :: forall e a. String -> Eff e a -> Eff e Unit

startTest :: forall e. Aff _ Unit
startTest = do
    let cacheOpts = C.host := "127.0.0.1" <> C.port := 6379 <> C.db := 0 <> C.socketKeepAlive := true
    cacheConn <- C.getConn cacheOpts
    {-- v <- C.setKey cacheConn "testing" "100" --}
    v0 <- C.lpop cacheConn "test-queue"
    _ <- traceShow v0 \_ -> pure unit
    v1 <- C.rpush cacheConn "test-queue" "hi"
    _ <- traceShow v1 \_ -> pure unit
    v2 <- C.lpop cacheConn "test-queue"
    _ <- traceShow v2 \_ -> pure unit
    v3 <- C.lpop cacheConn "test-queue"
    _ <- traceShow v3 \_ -> pure unit
    multi <- C.getMulti cacheConn
    val <- C.setKeyMulti "tt" "100" multi >>= C.setexKeyMulti "testing" "200" "1000" >>= C.incrMulti "testing" >>= C.getKeyMulti "tt" 
    val <- C.exec multi
    _ <- pure $ spy val
    -- v <- C.setex cacheConn "talk" "i am awesome" "10000"
    -- l <- C.enqueue cacheConn "DBACTIONS" "SELCT * FROM CUSTOMERS;"
    -- pop <- C.dequeue cacheConn "DBACTIONS"
    {-- peek <- C.getQueueIdx cacheConn "DBACTIONS" 0 --}
    {-- _ <- pure $ spy peek --}
    {-- _ <- pure $ spy $ "It worked" --}
    pure unit

main :: forall e. Eff _ Unit
main = startContext "awesome" (launchAff startTest) *> pure unit
