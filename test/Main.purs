module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff, launchAff)
import Cache (host, port, db, socketKeepAlive, getConn, CACHE, setex) as C
import Data.Options (options, (:=))
import Debug.Trace (spy)

foreign import startContext :: forall e a. String -> Eff e a -> Eff e Unit

startTest :: forall e. Aff _ Unit
startTest = do
    let cacheOpts = C.host := "127.0.0.1" <> C.port := 6379 <> C.db := 0 <> C.socketKeepAlive := true
    cacheConn <- C.getConn cacheOpts
    v <- C.setex cacheConn "talk" "i am awesome" "10000"
    _ <- pure $ spy v
    pure unit

main :: forall e. Eff _ Unit
main = startContext "awesome" (launchAff startTest) *> pure unit