module Cache.Hash
  ( hget
  , hset
  ) where

import Cache.Internal (isNotZero, readStringMaybe)
import Cache.Types (CacheConn)
import Control.Promise (Promise, toAff)
import Data.Either (Either)
import Data.Function.Uncurried (Fn3, Fn4, runFn3, runFn4)
import Data.Maybe (Maybe)
import Effect.Aff (Aff, attempt)
import Effect.Exception (Error)
import Foreign (Foreign)
import Prelude (map, ($), (<<<))

foreign import hgetJ :: Fn3 CacheConn String String (Promise Foreign)
foreign import hsetJ :: Fn4 CacheConn String String String (Promise Int)

hget :: CacheConn -> String -> String -> Aff (Either Error (Maybe String))
hget cacheConn key field = attempt <<< map readStringMaybe <<< toAff $ runFn3 hgetJ cacheConn key field

hset :: CacheConn -> String -> String -> String -> Aff (Either Error Boolean)
hset cacheConn key field value = attempt <<< map isNotZero <<< toAff $ runFn4 hsetJ cacheConn key field value