module Test.Multi where


import Prelude

import Cache (CacheConn)
import Cache.Multi (execMulti, getHashKeyMulti, newMulti, setHashMulti) as C
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Data.Options (options, (:=))
import Debug.Trace (spy, traceShow)

multiTest :: CacheConn -> Aff _ Unit
multiTest cacheConn = do
    let multi   = C.newMulti cacheConn
        multi'  = C.setHashMulti "myhash" "firstKey" "100" multi
        multi'' = C.getHashKeyMulti "myhash" "firstKey" multi'
    {-- val <- C.setKeyMulti "tt" "100" multi >>= C.setexKeyMulti "testing" "200" "1000" >>= C.incrMulti "testing" >>= C.getKeyMulti "tt" --}
    val <- C.execMulti multi''
    _ <- pure $ spy val
    pure unit
