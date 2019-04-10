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

module Cache
 ( module Cache.Types
 , db
 , del
 , exists
 , expire
 , getConn
 , getHashKey
 , get
 , host
 , incr
 , lindex
 , lpop
 , lpush
 , port
 , publishToChannel
 , retryStrategy
 , rpop
 , rpush
 , set
 , setHash
 , setMessageHandler
 , socketKeepAlive
 , subscribe
 , tryAfter
 , zipkinEnable
 , zipkinRedis
 , zipkinServiceName
 , zipkinURL
 ) where

import Cache.Types (CACHE, CacheAff, CacheConn, CacheConnOpts, CacheEff, SetOptions(..))
import Control.Monad.Aff (attempt)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Except (runExcept)
import Control.Promise (Promise, toAff, toAffE)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Either (Either, hush)
import Data.Foreign (Foreign, readString)
import Data.Function.Uncurried (Fn2, Fn3, Fn5, runFn2, runFn3, runFn5)
import Data.Int (round)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.Options (Option, Options, opt, options)
import Data.Time.Duration (Milliseconds, Seconds)
import Prelude (Unit, map, show, void, ($), (/=), (<<<))

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

foreign import setJ :: Fn5 CacheConn String String String String (Promise String)
foreign import getJ :: Fn2 CacheConn String (Promise Foreign)
foreign import existsJ :: Fn2 CacheConn String (Promise Int)
foreign import delJ :: Fn2 CacheConn (Array String) (Promise Int)
foreign import expireJ :: Fn3 CacheConn String Int (Promise Int)
foreign import incrJ :: CacheConn -> String -> Promise String
foreign import setHashJ :: CacheConn -> String -> String -> String -> Promise String
foreign import getHashKeyJ :: CacheConn -> String -> String -> Promise String
foreign import publishToChannelJ :: CacheConn -> String -> String -> Promise String
foreign import subscribeJ :: forall eff. CacheConn -> String -> Eff eff (Promise String)
foreign import setMessageHandlerJ :: forall eff1 eff2. CacheConn -> (String -> String -> Eff eff1 Unit) -> Eff eff2 (Promise String)
foreign import _newCache :: forall e. Foreign -> CacheEff e CacheConn
foreign import rpopJ :: CacheConn -> String -> Promise Foreign
foreign import rpushJ :: CacheConn -> String -> String -> Promise Int
foreign import lpopJ :: CacheConn -> String -> Promise Foreign
foreign import lpushJ :: CacheConn -> String -> String -> Promise Int
foreign import lindexJ :: CacheConn -> String -> Int -> Promise Foreign

isNotZero :: Int -> Boolean
isNotZero x = x /= 0

getConn :: forall e. Options CacheConnOpts -> CacheAff e CacheConn
getConn = liftEff <<< _newCache <<< options

set :: forall e. CacheConn -> String -> String -> Maybe Milliseconds -> SetOptions -> CacheAff e (Either Error Unit)
set cacheConn key value mExp opts =
  attempt <<< void <<< toAff $ runFn5 setJ cacheConn key value (maybe "" msToString mExp) (show opts)
  where
        msToString = show <<< round <<< unwrap

get :: forall e. CacheConn -> String -> CacheAff e (Either Error (Maybe String))
get cacheConn key = attempt <<< map readStringMaybe <<< toAff $ runFn2 getJ cacheConn key

exists :: forall e. CacheConn -> String -> CacheAff e (Either Error Boolean)
exists cacheConn = attempt <<< map isNotZero <<< toAff <<< runFn2 existsJ cacheConn

del :: forall e. CacheConn -> NonEmptyArray String -> CacheAff e (Either Error Int)
del cacheConn keys = attempt <<< toAff $ runFn2 delJ cacheConn (toArray keys)

expire :: forall e. CacheConn -> String -> Seconds -> CacheAff e (Either Error Boolean)
expire cacheConn key ttl = attempt <<< map isNotZero <<< toAff $ runFn3 expireJ cacheConn key (round <<< unwrap $ ttl)

incr :: forall e. CacheConn -> String -> CacheAff e  (Either Error String)
incr cacheConn key = attempt $ toAff $ incrJ cacheConn key

setHash :: forall e. CacheConn -> String -> String -> String -> CacheAff e  (Either Error String)
setHash cacheConn key field value = attempt $ toAff $ setHashJ cacheConn key field value

getHashKey :: forall e. CacheConn -> String -> String -> CacheAff e  (Either Error String)
getHashKey cacheConn key field = attempt $ toAff $ getHashKeyJ cacheConn key field

publishToChannel :: forall e. CacheConn -> String -> String -> CacheAff e  (Either Error String)
publishToChannel cacheConn channel message = attempt $ toAff $ publishToChannelJ cacheConn channel message

subscribe :: forall e. CacheConn -> String -> CacheAff e  (Either Error String)
subscribe cacheConn channel = attempt $ toAffE $ subscribeJ cacheConn channel

readStringMaybe :: Foreign -> Maybe String
readStringMaybe = hush <<< runExcept <<< readString

rpop :: forall e. CacheConn -> String -> CacheAff e (Either Error (Maybe String))
rpop cacheConn listName = attempt $ map readStringMaybe $ toAff $ rpopJ cacheConn listName

rpush :: forall e. CacheConn -> String -> String -> CacheAff e  (Either Error Int)
rpush cacheConn listName value = attempt $ toAff $ rpushJ cacheConn listName value

lpop :: forall e. CacheConn -> String -> CacheAff e (Either Error (Maybe String))
lpop cacheConn listName = attempt $ map readStringMaybe $ toAff $ lpopJ cacheConn listName

lpush :: forall e. CacheConn -> String -> String -> CacheAff e  (Either Error Int)
lpush cacheConn listName value = attempt $ toAff $ lpushJ cacheConn listName value

lindex :: forall e. CacheConn -> String -> Int -> CacheAff e  (Either Error (Maybe String))
lindex cacheConn listName index = attempt $ map readStringMaybe $ toAff $ lindexJ cacheConn listName index

setMessageHandler :: forall e eff. CacheConn -> (String -> String -> Eff eff Unit) -> CacheAff e  (Either Error String)
setMessageHandler cacheConn f = attempt $ toAffE $ setMessageHandlerJ cacheConn f
