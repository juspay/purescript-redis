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
 , duplicateConn
 , exists
 , expire
 , get
 , host
 , incr
 , incrby
 , newConn
 , port
 , publish
 , retryStrategy
 , set
 , setMessageHandler
 , socketKeepAlive
 , subscribe
 , tryAfter
 , zipkinEnable
 , zipkinRedis
 , zipkinServiceName
 , zipkinURL
 ) where

import Cache.Internal (isNotZero, readStringMaybe)
import Cache.Types (CacheConn, CacheConnOpts, SetOptions(..))
import Control.Promise (Promise, toAff)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, Fn3, Fn5, runFn2, runFn3, runFn5)
import Data.Int (round)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Options (Option, Options, opt, options)
import Data.Time.Duration (Milliseconds, Seconds)
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt)
import Foreign (Foreign, isNull)
import Foreign.NullOrUndefined (undefined)
import Prelude (Unit, map, show, void, ($), (<<<))

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

foreign import setJ :: Fn5 CacheConn String String String String (Promise Foreign)
foreign import getJ :: Fn2 CacheConn String (Promise Foreign)
foreign import existsJ :: Fn2 CacheConn String (Promise Int)
foreign import delJ :: Fn2 CacheConn (Array String) (Promise Int)
foreign import expireJ :: Fn3 CacheConn String Int (Promise Int)
foreign import incrJ :: Fn2 CacheConn String (Promise Int)
foreign import incrbyJ :: Fn3 CacheConn String Int (Promise Int)
foreign import publishJ :: Fn3 CacheConn String String (Promise Int)
foreign import subscribeJ :: Fn2 CacheConn (Array String) (Promise String)
foreign import setMessageHandlerJ :: Fn2 CacheConn (String -> String -> Effect Unit) (Effect Unit)
foreign import _newCache :: Foreign -> Promise CacheConn
foreign import _duplicateCache :: Fn2 CacheConn Foreign (Promise CacheConn)

newConn :: Options CacheConnOpts -> Aff (Either Error CacheConn)
newConn = attempt <<< toAff <<< _newCache <<< options

duplicateConn :: CacheConn -> Maybe (Options CacheConnOpts) -> Aff (Either Error CacheConn)
duplicateConn cacheConn Nothing     = attempt <<< toAff $ runFn2 _duplicateCache cacheConn undefined
duplicateConn cacheConn (Just opts) = attempt <<< toAff $ runFn2 _duplicateCache cacheConn (options opts)

set :: CacheConn -> String -> String -> Maybe Milliseconds -> SetOptions -> Aff (Either Error Boolean)
set cacheConn key value mExp opts =
  attempt <<< map parseSetResult <<< toAff $ runFn5 setJ cacheConn key value (maybe "" msToString mExp) (show opts)
  where
        msToString = show <<< round <<< unwrap

        parseSetResult res | isNull res = false -- Failed because exists (when NX) or not exists (when EX)
        parseSetResult res              = case readStringMaybe res of
                                               Just "OK" -> true  -- All good
                                               otherwise -> false -- This should not happen

get :: CacheConn -> String -> Aff (Either Error (Maybe String))
get cacheConn key = attempt <<< map readStringMaybe <<< toAff $ runFn2 getJ cacheConn key

exists :: CacheConn -> String -> Aff (Either Error Boolean)
exists cacheConn = attempt <<< map isNotZero <<< toAff <<< runFn2 existsJ cacheConn

del :: CacheConn -> NonEmptyArray String -> Aff (Either Error Int)
del cacheConn keys = attempt <<< toAff $ runFn2 delJ cacheConn (toArray keys)

expire :: CacheConn -> String -> Seconds -> Aff (Either Error Boolean)
expire cacheConn key ttl = attempt <<< map isNotZero <<< toAff $ runFn3 expireJ cacheConn key (round <<< unwrap $ ttl)

incr :: CacheConn -> String -> Aff (Either Error Int)
incr cacheConn key = attempt <<< toAff $ runFn2 incrJ cacheConn key

incrby :: CacheConn -> String -> Int -> Aff (Either Error Int)
incrby cacheConn key by = attempt <<< toAff $ runFn3 incrbyJ cacheConn key by

publish :: CacheConn -> String -> String -> Aff (Either Error Int)
publish cacheConn channel message = attempt <<< toAff $ runFn3 publishJ cacheConn channel message

subscribe :: CacheConn -> NonEmptyArray String -> Aff (Either Error Unit)
subscribe cacheConn channel = attempt <<< void <<< toAff $ runFn2 subscribeJ cacheConn (toArray channel)

setMessageHandler :: forall e eff. CacheConn -> (String -> String -> Effect Unit) -> Effect Unit
setMessageHandler cacheConn f = runFn2 setMessageHandlerJ cacheConn f
