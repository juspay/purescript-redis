module Test.Main where

import Prelude

import Cache (db, getConn, host, port, socketKeepAlive) as C
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Options ((:=))
import Test.Basic (basicTest)
import Test.Hash (hashTest)
import Test.List (listTest)
import Test.Multi (multiTest)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Test.Stream (streamTest)

startTest :: Aff _ Unit
startTest = do
    let cacheOpts = C.host := "127.0.0.1" <> C.port := 6379 <> C.db := 0 <> C.socketKeepAlive := true
    cacheConn <- C.getConn cacheOpts
    liftEff $ run [consoleReporter] do
       basicTest cacheConn
       listTest cacheConn
       hashTest cacheConn
       multiTest cacheConn
       streamTest cacheConn

main :: Eff _ Unit
main = launchAff startTest *> pure unit
