module Test.Queue where



import Prelude

import Cache (CacheConn)
import Cache (lindex, lpop, rpush, setex) as C
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Data.Options (options, (:=))
import Debug.Trace (spy, traceShow)
import Test.Multi (multiTest)


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
    v <- C.setex cacheConn "talk" "i am awesome" "10000"
    l <- C.rpush cacheConn "DBACTIONS" "SELCT * FROM CUSTOMERS;"
    pop <- C.lpop cacheConn "DBACTIONS"
    peek <- C.lindex cacheConn "DBACTIONS" 0
    _ <- pure $ spy peek
    _ <- pure $ spy $ "It worked"
    pure unit
