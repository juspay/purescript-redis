module Test.Main where

import Prelude

import Cache (dequeue, enqueue, getQueueIdx)
import Cache (host, port, db, socketKeepAlive, getConn, CACHE, setex, enqueue, dequeue, getQueueIdx) as C
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Data.Options (options, (:=))
import Debug.Trace (spy)

foreign import startContext :: forall e a. String -> Eff e a -> Eff e Unit

startTest :: forall e. Aff _ Unit
startTest = do
    let cacheOpts = C.host := "127.0.0.1" <> C.port := 6379 <> C.db := 0 <> C.socketKeepAlive := true
    cacheConn <- C.getConn cacheOpts
    -- v <- C.setex cacheConn "talk" "i am awesome" "10000"
    -- l <- C.enqueue cacheConn "DBACTIONS" "SELCT * FROM CUSTOMERS;"
    -- pop <- C.dequeue cacheConn "DBACTIONS"
    peek <- C.getQueueIdx cacheConn "DBACTIONS" 0
    _ <- pure $ spy peek
    _ <- pure $ spy $ "It worked"
    pure unit

main :: forall e. Eff _ Unit
main = startContext "awesome" (launchAff startTest) *> pure unit