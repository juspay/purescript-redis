{-
 Copyright (c) 2012-2017 "JUSPAY Technologies"
 JUSPAY Technologies Pvt. Ltd. [https://www.juspay.in]
 This file is part of JUSPAY Platform.
 JUSPAY Platform is free software: you can redistribute it and/or modify
 it for only educational purposes under the terms of the GNU Affero General
 Public License (GNU AGPL) as published by the Free Software Foundation,
 either version 3 of the License, or (at your option) any later version.
 For Enterprise/Commercial licenses, contact <info@juspay.in>.
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

import Control.Promise (Promise, toAffE)
import Data.Either (Either)
import Data.Options (Option, Options, opt, options)
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Foreign (Foreign)
import Prelude (($), (<<<), Unit)

foreign import data CacheConn :: Type

data CacheConnOpts

host :: Option CacheConnOpts String
host = opt "host"

port :: Option CacheConnOpts Int
port = opt "port"

db :: Option CacheConnOpts Int
db = opt "db"

socketKeepAlive :: Option CacheConnOpts Boolean
socketKeepAlive = opt "socket_keepalive"

sentinels :: Option CacheConnOpts (Array { host :: String, port :: Int })
sentinels = opt "sentinels"

name :: Option CacheConnOpts String
name = opt "name"

retryStrategy :: Option CacheConnOpts (CacheConnOpts -> Int)
retryStrategy = opt "retryStrategy"

logger :: Option CacheConnOpts Foreign
logger = opt "logger"

foreign import setKeyJ :: CacheConn -> String -> String -> Effect (Promise String)
foreign import getKeyJ :: CacheConn -> String -> Effect (Promise String)
foreign import setexJ :: CacheConn -> String -> String -> String -> Effect (Promise String)
foreign import delKeyJ :: CacheConn -> Array String -> Effect (Promise String)
foreign import expireJ :: CacheConn -> String -> String -> Effect (Promise String)
foreign import incrJ :: CacheConn -> String -> Effect (Promise String)
foreign import setHashJ :: CacheConn -> String -> String -> Effect (Promise String)
foreign import getHashKeyJ :: CacheConn -> String -> String -> Effect (Promise String)
foreign import publishToChannelJ :: CacheConn -> String -> String -> Effect (Promise String)
foreign import subscribeJ :: CacheConn -> String -> Effect (Promise String)
foreign import setMessageHandlerJ :: CacheConn -> (String -> String -> Effect Unit) -> Effect (Promise String)
foreign import _newCache :: Foreign -> Effect CacheConn

getConn :: Options CacheConnOpts -> Aff CacheConn
getConn = liftEffect <<< _newCache <<< options

setKey :: CacheConn -> String -> String -> Aff (Either Error String)
setKey cacheConn key value = attempt $ toAffE $ setKeyJ cacheConn key value

setex :: CacheConn -> String -> String -> String -> Aff (Either Error String)
setex cacheConn key value ttl = attempt $ toAffE $ setexJ cacheConn key value ttl

getKey :: CacheConn -> String -> Aff (Either Error String)
getKey cacheConn key = attempt $ toAffE $ getKeyJ cacheConn key

delKey :: CacheConn -> String -> Aff (Either Error String)
delKey cacheConn key = attempt $ toAffE $ delKeyJ cacheConn [key]

delKeyList :: CacheConn -> Array String -> Aff (Either Error String)
delKeyList cacheConn key = attempt $ toAffE $ delKeyJ cacheConn key

expire :: CacheConn -> String -> String -> Aff (Either Error String)
expire cacheConn key ttl = attempt $ toAffE $ expireJ cacheConn key ttl

incr :: CacheConn -> String -> Aff (Either Error String)
incr cacheConn key = attempt $ toAffE $ incrJ cacheConn key

setHash :: CacheConn -> String -> String -> Aff (Either Error String)
setHash cacheConn key value = attempt $ toAffE $ setHashJ cacheConn key value

getHashKey :: CacheConn -> String -> String -> Aff (Either Error String)
getHashKey cacheConn key field = attempt $ toAffE $ getHashKeyJ cacheConn key field

publishToChannel :: CacheConn -> String -> String -> Aff (Either Error String)
publishToChannel cacheConn channel message = attempt $ toAffE $ publishToChannelJ cacheConn channel message

subscribe :: CacheConn -> String -> Aff (Either Error String)
subscribe cacheConn channel = attempt $ toAffE $ subscribeJ cacheConn channel

setMessageHandler :: CacheConn -> (String -> String -> Effect Unit) -> Aff (Either Error String)
setMessageHandler cacheConn f = attempt $ toAffE $ setMessageHandlerJ cacheConn f
