module Cache.Hash
  ( hget
  , hset
  ) where

import Cache.Internal (isNotZero, readStringMaybe)
import Cache.Types (class CacheConn, CacheAff)
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (Error)
import Control.Promise (Promise, toAff)
import Data.Either (Either)
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn3, Fn4, runFn3, runFn4)
import Data.Maybe (Maybe)
import Prelude (map, ($), (<<<))

foreign import hgetJ :: forall a. Fn3 a String String (Promise Foreign)
foreign import hsetJ :: forall a. Fn4 a String String String (Promise Int)

hget :: forall a e. CacheConn a => a -> String -> String -> CacheAff e (Either Error (Maybe String))
hget cacheConn key field = attempt <<< map readStringMaybe <<< toAff $ runFn3 hgetJ cacheConn key field

hset :: forall a e. CacheConn a => a -> String -> String -> String -> CacheAff e (Either Error Boolean)
hset cacheConn key field value = attempt <<< map isNotZero <<< toAff $ runFn4 hsetJ cacheConn key field value
