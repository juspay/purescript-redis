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
 ) where

import Cache.Internal (isNotZero, readStringMaybe)
import Cache.Types (class CacheConn, CACHE, CacheAff, CacheConnOpts, CacheEff, SetOptions(..), SimpleConn)
import Control.Monad.Aff (attempt)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (Error)
import Control.Promise (Promise, toAff)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Either (Either)
import Data.Foreign (Foreign, isNull)
import Data.Foreign.NullOrUndefined (undefined)
import Data.Function.Uncurried (Fn2, Fn3, Fn5, runFn2, runFn3, runFn5)
import Data.Int (round)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Options (Option, Options, opt, options)
import Data.Time.Duration (Milliseconds, Seconds)
import Prelude (Unit, map, show, void, ($), (<<<))

host :: Option CacheConnOpts String
host = opt "host"

port :: Option CacheConnOpts Int
port = opt "port"

db :: Option CacheConnOpts Int
db = opt "db"

socketKeepAlive :: Option CacheConnOpts Boolean
socketKeepAlive = opt "keepAlive"

retryStrategy :: Option CacheConnOpts (CacheConnOpts -> Int)
retryStrategy = opt "retryStrategy"

foreign import setJ :: forall a. Fn5 a String String String String (Promise Foreign)
foreign import getJ :: forall a. Fn2 a String (Promise Foreign)
foreign import existsJ :: forall a. Fn2 a String (Promise Int)
foreign import delJ :: forall a. Fn2 a (Array String) (Promise Int)
foreign import expireJ :: forall a. Fn3 a String Int (Promise Int)
foreign import incrJ :: forall a. Fn2 a String (Promise Int)
foreign import incrbyJ :: forall a. Fn3 a String Int (Promise Int)
foreign import publishJ :: forall a. Fn3 a String String (Promise Int)
foreign import subscribeJ :: forall a. Fn2 a (Array String) (Promise String)
foreign import setMessageHandlerJ :: forall a e eff. Fn2 a (String -> String -> Eff eff Unit) (CacheEff e Unit)
foreign import _newCache :: Foreign -> Promise SimpleConn
foreign import _duplicateCache :: Fn2 SimpleConn Foreign (Promise SimpleConn)

newConn :: forall e. Options CacheConnOpts -> CacheAff e (Either Error SimpleConn)
newConn = attempt <<< toAff <<< _newCache <<< options

duplicateConn :: forall e. SimpleConn -> Maybe (Options CacheConnOpts) -> CacheAff e (Either Error SimpleConn)
duplicateConn cacheConn Nothing     = attempt <<< toAff $ runFn2 _duplicateCache cacheConn undefined
duplicateConn cacheConn (Just opts) = attempt <<< toAff $ runFn2 _duplicateCache cacheConn (options opts)

set :: forall a e. CacheConn a => a -> String -> String -> Maybe Milliseconds -> SetOptions -> CacheAff e (Either Error Boolean)
set cacheConn key value mExp opts =
  attempt <<< map parseSetResult <<< toAff $ runFn5 setJ cacheConn key value (maybe "" msToString mExp) (show opts)
  where
        msToString = show <<< round <<< unwrap

        parseSetResult res | isNull res = false -- Failed because exists (when NX) or not exists (when EX)
        parseSetResult res              = case readStringMaybe res of
                                               Just "OK" -> true  -- All good
                                               otherwise -> false -- This should not happen

get :: forall a e. CacheConn a => a -> String -> CacheAff e (Either Error (Maybe String))
get cacheConn key = attempt <<< map readStringMaybe <<< toAff $ runFn2 getJ cacheConn key

exists :: forall a e. CacheConn a => a -> String -> CacheAff e (Either Error Boolean)
exists cacheConn = attempt <<< map isNotZero <<< toAff <<< runFn2 existsJ cacheConn

del :: forall a e. CacheConn a => a -> NonEmptyArray String -> CacheAff e (Either Error Int)
del cacheConn keys = attempt <<< toAff $ runFn2 delJ cacheConn (toArray keys)

expire :: forall a e. CacheConn a => a -> String -> Seconds -> CacheAff e (Either Error Boolean)
expire cacheConn key ttl = attempt <<< map isNotZero <<< toAff $ runFn3 expireJ cacheConn key (round <<< unwrap $ ttl)

incr :: forall a e. CacheConn a => a -> String -> CacheAff e (Either Error Int)
incr cacheConn key = attempt <<< toAff $ runFn2 incrJ cacheConn key

incrby :: forall a e. CacheConn a => a -> String -> Int -> CacheAff e (Either Error Int)
incrby cacheConn key by = attempt <<< toAff $ runFn3 incrbyJ cacheConn key by

publish :: forall a e. CacheConn a => a -> String -> String -> CacheAff e (Either Error Int)
publish cacheConn channel message = attempt <<< toAff $ runFn3 publishJ cacheConn channel message

subscribe :: forall a e. CacheConn a => a -> NonEmptyArray String -> CacheAff e (Either Error Unit)
subscribe cacheConn channel = attempt <<< void <<< toAff $ runFn2 subscribeJ cacheConn (toArray channel)

setMessageHandler :: forall a e eff. CacheConn a => a -> (String -> String -> Eff eff Unit) -> CacheEff e Unit
setMessageHandler cacheConn f = runFn2 setMessageHandlerJ cacheConn f
