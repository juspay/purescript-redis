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
import Control.Promise (Promise, toAff, toAffE)
import Data.Either (Either)
import Data.Foreign (Foreign)
import Data.Options (Option, Options, opt, options)
import Prelude (Unit, pure, ($), (<<<))
foreign import data CacheConn :: Type

foreign import data Multi :: Type

foreign import data CACHE :: Effect

data CacheConnOpts

type CacheEff e = Eff (cache :: CACHE | e)
type CacheAff e = Aff (cache :: CACHE | e)
type MulTiToMulTi = Multi -> Multi

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

foreign import setJ :: CacheConn -> Array String -> Promise String
foreign import setKeyJ :: CacheConn -> String -> String -> Promise String
foreign import getKeyJ :: CacheConn -> String -> Promise String
foreign import setexJ :: CacheConn -> String -> String -> String -> Promise String
foreign import delKeyJ :: CacheConn -> Array String -> Promise String
foreign import expireJ :: CacheConn -> String -> String -> Promise String
foreign import incrJ :: CacheConn -> String -> Promise String
foreign import setHashJ :: CacheConn -> String -> String -> Promise String
foreign import getHashKeyJ :: CacheConn -> String -> String -> Promise String
foreign import publishToChannelJ :: CacheConn -> String -> String -> Promise String
foreign import subscribeJ :: forall eff. CacheConn -> String -> Eff eff (Promise String)
foreign import setMessageHandlerJ :: forall eff1 eff2. CacheConn -> (String -> String -> Eff eff1 Unit) -> Eff eff2 (Promise String)
foreign import _newCache :: forall e. Foreign -> CacheEff e CacheConn
foreign import _newMulti :: forall e. CacheConn -> CacheEff e Multi
foreign import execMulti :: Multi -> Promise (Array String)
foreign import enqueueJ :: CacheConn -> String -> String -> Promise String
foreign import dequeueJ :: CacheConn -> String -> Promise String
foreign import getQueueIdxJ :: CacheConn -> String -> Int -> Promise String

foreign import setMultiJ ::  Array String -> MulTiToMulTi  
foreign import getKeyMultiJ ::  String -> MulTiToMulTi
foreign import setKeyMultiJ ::  String -> String -> MulTiToMulTi
foreign import setexKeyMultiJ :: String -> String -> String -> MulTiToMulTi
foreign import delKeyMultiJ :: Array String -> MulTiToMulTi
foreign import expireMultiJ :: String -> String -> MulTiToMulTi
foreign import incrMultiJ ::  String -> MulTiToMulTi
foreign import setHashMultiJ :: String -> String -> MulTiToMulTi
foreign import getHashMultiJ :: String -> String -> MulTiToMulTi 
foreign import publishCMultiJ :: String -> String -> MulTiToMulTi
foreign import subscribeMultiJ :: String -> MulTiToMulTi
foreign import enqueueMultiJ :: String -> String -> MulTiToMulTi
foreign import dequeueMultiJ :: String -> MulTiToMulTi
foreign import getQueueIdxMultiJ :: String -> Int -> MulTiToMulTi

getConn :: forall e. Options CacheConnOpts -> CacheAff e CacheConn
getConn = liftEff <<< _newCache <<< options

getMulti :: forall e. CacheConn -> CacheAff e Multi
getMulti = liftEff <<< _newMulti 

exec :: forall e. Multi -> CacheAff e (Either Error (Array String))
exec = attempt <<< toAff <<< execMulti

setMulti :: forall e. Array String -> Multi -> CacheAff e Multi
setMulti vals =  pure <<< setMultiJ vals

set :: forall e. CacheConn -> Array String -> Aff (cache :: CACHE | e ) (Either Error String)
set cacheConn arr = attempt $ toAff $ setJ cacheConn arr

setKeyMulti :: forall e. String -> String -> Multi -> CacheAff e Multi
setKeyMulti key val =  pure <<< setKeyMultiJ key val

setKey :: forall e. CacheConn -> String -> String -> CacheAff e  (Either Error String)
setKey cacheConn key value = attempt $ toAff $ setKeyJ cacheConn key value

setexKeyMulti :: forall e. String -> String -> String -> Multi -> CacheAff e Multi
setexKeyMulti key value ttl =  pure <<< setexKeyMultiJ key value ttl 

setex :: forall e. CacheConn -> String -> String -> String -> CacheAff e  (Either Error String)
setex cacheConn key value ttl = attempt $ toAff $ setexJ cacheConn key value ttl

getKeyMulti :: forall e. String -> Multi -> CacheAff e Multi
getKeyMulti val =  pure <<< getKeyMultiJ val

getKey :: forall e. CacheConn -> String -> CacheAff e  (Either Error String)
getKey cacheConn key = attempt $ toAff $ getKeyJ cacheConn key

delKeyMulti :: forall e. String -> Multi -> CacheAff e Multi
delKeyMulti key = pure <<< delKeyMultiJ [key]

delKey :: forall e. CacheConn -> String -> CacheAff e  (Either Error String)
delKey cacheConn key = attempt $ toAff $ delKeyJ cacheConn [key]

delKeyListMulti :: forall e. Array String -> Multi -> CacheAff e Multi
delKeyListMulti keys =  pure <<< delKeyMultiJ keys 

delKeyList :: forall e. CacheConn -> Array String -> CacheAff e  (Either Error String)
delKeyList cacheConn key = attempt $ toAff $ delKeyJ cacheConn key

expireMulti :: forall e. String -> String -> Multi -> CacheAff e Multi
expireMulti key ttl = pure <<< expireMultiJ key ttl

expire :: forall e. CacheConn -> String -> String -> CacheAff e  (Either Error String)
expire cacheConn key ttl = attempt $ toAff $ expireJ cacheConn key ttl

incrMulti :: forall e. String -> Multi -> CacheAff e Multi
incrMulti key = pure <<< incrMultiJ key 

incr :: forall e. CacheConn -> String -> CacheAff e  (Either Error String)
incr cacheConn key = attempt $ toAff $ incrJ cacheConn key

setHashMulti :: forall e. String -> String -> Multi -> CacheAff e Multi
setHashMulti key val = pure <<< setHashMultiJ key val

setHash :: forall e. CacheConn -> String -> String -> CacheAff e  (Either Error String)
setHash cacheConn key value = attempt $ toAff $ setHashJ cacheConn key value

getHashKeyMulti :: forall e. String -> String -> Multi -> CacheAff e Multi
getHashKeyMulti key field = pure <<< getHashMultiJ key field

getHashKey :: forall e. CacheConn -> String -> String -> CacheAff e  (Either Error String)
getHashKey cacheConn key field = attempt $ toAff $ getHashKeyJ cacheConn key field

publishToChannelMulti :: forall e. String -> String -> Multi ->  CacheAff e Multi
publishToChannelMulti channel message = pure <<< publishCMultiJ channel message

publishToChannel :: forall e. CacheConn -> String -> String -> CacheAff e  (Either Error String)
publishToChannel cacheConn channel message = attempt $ toAff $ publishToChannelJ cacheConn channel message

subscribeMulti :: forall e. String -> Multi -> CacheAff e Multi
subscribeMulti channel =  pure <<< subscribeMultiJ channel

subscribe :: forall e. CacheConn -> String -> CacheAff e  (Either Error String)
subscribe cacheConn channel = attempt $ toAffE $ subscribeJ cacheConn channel

enqueueMulti :: forall e. String -> String -> Multi -> CacheAff e Multi
enqueueMulti listName value = pure <<< enqueueMultiJ listName value

enqueue :: forall e. CacheConn -> String -> String -> CacheAff e  (Either Error String)
enqueue cacheConn listName value = attempt $ toAff $ enqueueJ cacheConn listName value

dequeueMulti :: forall e. String -> Multi -> CacheAff e Multi
dequeueMulti listName = pure <<< dequeueMultiJ listName

dequeue :: forall e. CacheConn -> String -> CacheAff e  (Either Error String)
dequeue cacheConn listName = attempt $ toAff $ dequeueJ cacheConn listName

getQueueIdxMulti :: forall e. String -> Int -> Multi -> CacheAff e Multi
getQueueIdxMulti listName index = pure <<< getQueueIdxMultiJ  listName index

getQueueIdx :: forall e. CacheConn -> String -> Int -> CacheAff e  (Either Error String)
getQueueIdx cacheConn listName index = attempt $ toAff $ getQueueIdxJ cacheConn listName index

setMessageHandler :: forall e eff. CacheConn -> (String -> String -> Eff eff Unit) -> CacheAff e  (Either Error String)
setMessageHandler cacheConn f = attempt $ toAffE $ setMessageHandlerJ cacheConn f

