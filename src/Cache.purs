{-
 Copyright (c) 2012-2017 "JUSPAY Technologies"
 JUSPAY Technologies Pvt. Ltd. [https://www.juspay.in]
 This file is part of JUSPAY Platform.
 JUSPAY Platform is free software: you can redistribute it and/or modify
 it for only educational purposes under the terms of the GNU Affero General
 Public License (GNU AGPL) as published by the Free Software Foundation,
 either version 3 of the License, or (at your option) any later version.
 For Enterprise/Commerical licenses, contact <info@juspay.in>.
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  The end user will
 be liable for all damages without limitation, which is caused by the
 ABUSE of the LICENSED SOFTWARE and shall INDEMNIFY JUSPAY for such
 damages, claims, cost, including reasonable attorney fee claimed on Juspay.
 The end user has NO right to claim any indemnification based on its use
 of Licensed Software. See the GNU Affero General Public License for more details.
 You should have received a copy of the GNU Affero General Public License
 along with this program. If not, see <https://www.gnu.org/licenses/agpl.html>.
-}

module Cache where

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Promise (Promise, toAff)
import Data.Either (Either)
import Data.Foreign (Foreign)
import Data.Options (Option, Options, opt, options)
import Prelude (($), (<<<), Unit)

foreign import data CacheConn :: Type

foreign import data CACHE :: Effect

data CacheConnOpts

host :: Option CacheConnOpts String
host = opt "host"

port :: Option CacheConnOpts Int
port = opt "port"

db :: Option CacheConnOpts Int
db = opt "db"

socketKeepAlive :: Option CacheConnOpts Boolean
socketKeepAlive = opt "socket_keepalive"

-- sentinels :: Option CacheConnOpts (Array { host :: String, port :: Int })
-- sentinels = opt "sentinels"

-- name :: Option CacheConnOpts String
-- name = opt "name"

tryAfter :: Option CacheConnOpts Int
tryAfter = opt "try_after"

retryStrategy :: Option CacheConnOpts (CacheConnOpts -> Int)
retryStrategy = opt "retry_strategy"
-- retryStrategy = opt "retryStrategy"

zipkinEnable :: Option CacheConnOpts String
zipkinEnable = opt "zipkinEnable"

zipkinRedis :: Option CacheConnOpts String
zipkinRedis = opt "zipkinRedis"

zipkinURL :: Option CacheConnOpts String
zipkinURL = opt "zipkinURL"

zipkinServiceName :: Option CacheConnOpts String
zipkinServiceName = opt "zipkinServiceName"

foreign import setKeyJ :: CacheConn -> String -> String -> Promise String
foreign import getKeyJ :: CacheConn -> String -> Promise String
foreign import setexJ :: CacheConn -> String -> String -> String -> Promise String
foreign import delKeyJ :: CacheConn -> Array String -> Promise String
foreign import expireJ :: CacheConn -> String -> String -> Promise String
foreign import incrJ :: CacheConn -> String -> Promise String
foreign import setHashJ :: CacheConn -> String -> String -> Promise String
foreign import getHashKeyJ :: CacheConn -> String -> String -> Promise String
foreign import publishToChannelJ :: CacheConn -> String -> String -> Promise String
foreign import subscribeJ :: CacheConn -> String -> Promise String
foreign import setMessageHandlerJ :: CacheConn -> (String -> String -> Unit) -> Promise String
foreign import _newCache :: forall e. Foreign -> Eff ( cache :: CACHE | e ) CacheConn

getConn :: forall e. Options CacheConnOpts -> Aff ( cache :: CACHE | e ) CacheConn
getConn = liftEff <<< _newCache <<< options

setKey :: forall e. CacheConn -> String -> String -> Aff (cache :: CACHE | e) (Either Error String)
setKey cacheConn key value = attempt $ toAff $ setKeyJ cacheConn key value

setex :: forall e. CacheConn -> String -> String -> String -> Aff (cache :: CACHE | e) (Either Error String)
setex cacheConn key value ttl = attempt $ toAff $ setexJ cacheConn key value ttl

getKey :: forall e. CacheConn -> String -> Aff (cache :: CACHE | e) (Either Error String)
getKey cacheConn key = attempt $ toAff $ getKeyJ cacheConn key

delKey :: forall e. CacheConn -> String -> Aff (cache :: CACHE | e) (Either Error String)
delKey cacheConn key = attempt $ toAff $ delKeyJ cacheConn [key]

delKeyList :: forall e. CacheConn -> Array String -> Aff (cache :: CACHE | e) (Either Error String)
delKeyList cacheConn key = attempt $ toAff $ delKeyJ cacheConn key

expire :: forall e. CacheConn -> String -> String -> Aff (cache :: CACHE | e) (Either Error String)
expire cacheConn key ttl = attempt $ toAff $ expireJ cacheConn key ttl

incr :: forall e. CacheConn -> String -> Aff (cache :: CACHE | e) (Either Error String)
incr cacheConn key = attempt $ toAff $ incrJ cacheConn key

setHash :: forall e. CacheConn -> String -> String -> Aff (cache :: CACHE | e) (Either Error String)
setHash cacheConn key value = attempt $ toAff $ setHashJ cacheConn key value

getHashKey :: forall e. CacheConn -> String -> String -> Aff (cache :: CACHE | e) (Either Error String)
getHashKey cacheConn key field = attempt $ toAff $ getHashKeyJ cacheConn key field

publishToChannel :: forall e. CacheConn -> String -> String -> Aff (cache :: CACHE | e) (Either Error String)
publishToChannel cacheConn channel message = attempt $ toAff $ publishToChannelJ cacheConn channel message

subscribe :: forall e. CacheConn -> String -> Aff (cache :: CACHE | e) (Either Error String)
subscribe cacheConn channel = attempt $ toAff $ subscribeJ cacheConn channel

setMessageHandler :: forall e. CacheConn -> (String -> String -> Unit) -> Aff (cache :: CACHE | e) (Either Error String)
setMessageHandler cacheConn f = attempt $ toAff $ setMessageHandlerJ cacheConn f

