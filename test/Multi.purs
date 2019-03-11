module Test.Multi where


import Prelude

import Cache (CACHE, db, exec, getConn, getHashKey, getHashKeyMulti, getKey, getKeyMulti, getMulti, host, incr, incrMulti, lpop, port, rpush, setHash, setHashMulti, setKey, setKeyMulti, setMulti, setex, setexKeyMulti, socketKeepAlive) as C
import Cache (CacheConn)
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Data.Options (options, (:=))
import Debug.Trace (spy, traceShow)


multiTest :: CacheConn -> Aff _ Unit
multiTest cacheConn = do
    multi <- C.getMulti cacheConn
    multi <- C.setHashMulti "myhash" "firstKey" "100" multi
    val <- C.getHashKeyMulti "myhash" "firstKey" multi
    {-- val <- C.setKeyMulti "tt" "100" multi >>= C.setexKeyMulti "testing" "200" "1000" >>= C.incrMulti "testing" >>= C.getKeyMulti "tt" --} 
    val <- C.exec multi
    _ <- pure $ spy val
    pure unit
