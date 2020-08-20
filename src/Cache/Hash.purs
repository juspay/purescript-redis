module Cache.Hash
  ( hget
  , hset
  , hdel
  , hsetnx
  , hgetall
  , hincrby
  , hmget
  , hmset
  ) where

import Cache.Internal (flattenArr, isNotZero, isOk, readStringMaybe)
import Cache.Types (class CacheConn)
import Control.Promise (Promise, toAff)
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Effect.Aff (Aff, Error, attempt)
import Foreign (Foreign)
import Foreign.Object (Object)
import Prelude (map, ($), (<<<))

foreign import hgetJ :: forall a. Fn3 a String String (Promise Foreign)
foreign import hmgetJ :: forall a. Fn3 a String (Array String) (Promise (Array Foreign))
foreign import hgetallJ :: forall a. Fn2 a String (Promise (Object String))
foreign import hsetJ :: forall a. Fn4 a String String String (Promise Int)
foreign import hdelJ :: forall a. Fn3 a String String (Promise Int)
foreign import hmsetJ :: forall a. Fn3 a String (Array String) (Promise String)
foreign import hsetnxJ :: forall a. Fn4 a String String String (Promise Int)
foreign import hincrbyJ :: forall a. Fn4 a String String Int (Promise Int)

hget :: forall a. CacheConn a => a -> String -> String -> Aff (Either Error (Maybe String))
hget cacheConn key field = attempt <<< map readStringMaybe <<< toAff $ runFn3 hgetJ cacheConn key field

hmget :: forall a. CacheConn a => a -> String -> (Array String) -> Aff (Either Error (Array (Maybe String)))
hmget cacheConn key fieldArr = attempt <<< map (map readStringMaybe) <<< toAff $ runFn3 hmgetJ cacheConn key fieldArr

hgetall :: forall a. CacheConn a => a -> String -> Aff (Either Error (Object String))
hgetall cacheConn key = attempt <<< toAff $ runFn2 hgetallJ cacheConn key

hset :: forall a. CacheConn a => a -> String -> String -> String -> Aff (Either Error Boolean)
hset cacheConn key field value = attempt <<< map isNotZero <<< toAff $ runFn4 hsetJ cacheConn key field value

hdel :: forall a. CacheConn a => a -> String -> String -> Aff (Either Error Boolean)
hdel cacheConn key field = attempt <<< map isNotZero <<< toAff $ runFn3 hdelJ cacheConn key field

hmset :: forall a. CacheConn a => a -> String -> Array (Tuple String String) -> Aff (Either Error Boolean)
hmset cacheConn key valArr = attempt <<< map isOk <<< toAff $ runFn3 hmsetJ cacheConn key (flattenArr valArr)

hsetnx :: forall a. CacheConn a => a -> String -> String -> String -> Aff (Either Error Boolean)
hsetnx cacheConn key field value = attempt <<< map isNotZero <<< toAff $ runFn4 hsetnxJ cacheConn key field value

hincrby :: forall a. CacheConn a => a -> String -> String -> Int -> Aff (Either Error Int)
hincrby cacheConn key field value = attempt <<< toAff $ runFn4 hincrbyJ cacheConn key field value
