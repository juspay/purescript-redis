module Test.Queue where

import Prelude

import Cache (CacheConn, SetOptions(..))
import Cache (lindex, lpop, rpush, set) as C
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Debug.Trace (spy, traceShow)

queueTest :: CacheConn -> Aff _ Unit
queueTest cacheConn = do
    v0 <- C.lpop cacheConn "test-queue"
    _ <- traceShow v0 \_ -> pure unit
    v1 <- C.rpush cacheConn "test-queue" "hi"
    _ <- traceShow v1 \_ -> pure unit
    v2 <- C.lpop cacheConn "test-queue"
    _ <- traceShow v2 \_ -> pure unit
    v3 <- C.lpop cacheConn "test-queue"
    _ <- traceShow v3 \_ -> pure unit
    v <- C.set cacheConn "talk" "i am awesome" (Just $ Milliseconds 10000.0) NoOptions
    l <- C.rpush cacheConn "DBACTIONS" "SELCT * FROM CUSTOMERS;"
    pop <- C.lpop cacheConn "DBACTIONS"
    peek <- C.lindex cacheConn "DBACTIONS" 0
    _ <- pure $ spy peek
    _ <- pure $ spy $ "It worked"
    pure unit
