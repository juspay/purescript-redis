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
 , getTTL
 , host
 , incr
 , incrby
 , newConn
 , password
 , port
 , publish
 , retryStrategy
 , set
 , setMessageHandler
 , socketKeepAlive
 , subscribe
 ) where

import Cache.Internal (isNotZero, readStringMaybe)
import Cache.Types (class CacheConn, SetOptions(..), SimpleConn, SimpleConnOpts)
import Control.Promise (Promise, toAff, toAffE)
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

host :: Option SimpleConnOpts String
host = opt "host"

port :: Option SimpleConnOpts Int
port = opt "port"

db :: Option SimpleConnOpts Int
db = opt "db"

password :: Option SimpleConnOpts String
password = opt "password"

socketKeepAlive :: Option SimpleConnOpts Boolean
socketKeepAlive = opt "keepAlive"

retryStrategy :: Option SimpleConnOpts (SimpleConnOpts -> Int)
retryStrategy = opt "retryStrategy"

foreign import setJ :: forall a. Fn5 a String String String String (Promise Foreign)
foreign import getJ :: forall a. Fn2 a String (Promise Foreign)
foreign import getTTLJ :: forall a. Fn2 a String (Effect (Promise Int))
foreign import existsJ :: forall a. Fn2 a String (Promise Int)
foreign import delJ :: forall a. Fn2 a (Array String) (Promise Int)
foreign import expireJ :: forall a. Fn3 a String Int (Promise Int)
foreign import incrJ :: forall a. Fn2 a String (Promise Int)
foreign import incrbyJ :: forall a. Fn3 a String Int (Promise Int)
foreign import publishJ :: forall a. Fn3 a String String (Promise Int)
foreign import subscribeJ :: forall a. Fn2 a (Array String) (Promise String)
foreign import setMessageHandlerJ :: forall a. Fn2 a (String -> String -> Effect Unit) (Effect Unit)
foreign import _newCache :: Foreign -> Promise SimpleConn
foreign import _duplicateCache :: Fn2 SimpleConn Foreign (Promise SimpleConn)

newConn :: Options SimpleConnOpts -> Aff (Either Error SimpleConn)
newConn = attempt <<< toAff <<< _newCache <<< options

duplicateConn :: SimpleConn -> Maybe (Options SimpleConnOpts) -> Aff (Either Error SimpleConn)
duplicateConn cacheConn Nothing     = attempt <<< toAff $ runFn2 _duplicateCache cacheConn undefined
duplicateConn cacheConn (Just opts) = attempt <<< toAff $ runFn2 _duplicateCache cacheConn (options opts)

set :: forall a. CacheConn a => a -> String -> String -> Maybe Milliseconds -> SetOptions -> Aff (Either Error Boolean)
set cacheConn key value mExp opts =
  attempt <<< map parseSetResult <<< toAff $ runFn5 setJ cacheConn key value (maybe "" msToString mExp) (show opts)
  where
        msToString = show <<< round <<< unwrap
        parseSetResult res | isNull res = false -- Failed because exists (when NX) or not exists (when EX)
        parseSetResult res              = case readStringMaybe res of
                                               Just "OK" -> true  -- All good
                                               otherwise -> false -- This should not happen

get :: forall a. CacheConn a => a -> String -> Aff (Either Error (Maybe String))
get cacheConn key = attempt <<< map readStringMaybe <<< toAff $ runFn2 getJ cacheConn key

getTTL :: forall a. CacheConn a => a -> String -> Aff (Either Error Int)
getTTL cacheConn key = attempt <<< toAffE $ runFn2 getTTLJ cacheConn key

exists :: forall a. CacheConn a => a -> String -> Aff (Either Error Boolean)
exists cacheConn = attempt <<< map isNotZero <<< toAff <<< runFn2 existsJ cacheConn

del :: forall a. CacheConn a => a -> NonEmptyArray String -> Aff (Either Error Int)
del cacheConn keys = attempt <<< toAff $ runFn2 delJ cacheConn (toArray keys)

expire :: forall a. CacheConn a => a -> String -> Seconds -> Aff (Either Error Boolean)
expire cacheConn key ttl = attempt <<< map isNotZero <<< toAff $ runFn3 expireJ cacheConn key (round <<< unwrap $ ttl)

incr :: forall a. CacheConn a => a -> String -> Aff (Either Error Int)
incr cacheConn key = attempt <<< toAff $ runFn2 incrJ cacheConn key

incrby :: forall a. CacheConn a => a -> String -> Int -> Aff (Either Error Int)
incrby cacheConn key by = attempt <<< toAff $ runFn3 incrbyJ cacheConn key by

publish :: forall a. CacheConn a => a -> String -> String -> Aff (Either Error Int)
publish cacheConn channel message = attempt <<< toAff $ runFn3 publishJ cacheConn channel message

subscribe :: forall a. CacheConn a => a -> NonEmptyArray String -> Aff (Either Error Unit)
subscribe cacheConn channel = attempt <<< void <<< toAff $ runFn2 subscribeJ cacheConn (toArray channel)

setMessageHandler :: forall a. CacheConn a => a -> (String -> String -> Effect Unit) -> Effect Unit
setMessageHandler cacheConn f = runFn2 setMessageHandlerJ cacheConn f
