module Cache.Hash
  ( hget
  , hset
  ) where

import Cache.Internal (isNotZero, readStringMaybe)
import Cache.Types (CacheConn, CacheAff)
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (Error)
import Control.Promise (Promise, toAff)
import Data.Either (Either)
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn3, Fn4, runFn3, runFn4)
import Data.Maybe (Maybe)
import Prelude (map, ($), (<<<))

foreign import hgetJ :: Fn3 CacheConn String String (Promise Foreign)
foreign import hsetJ :: Fn4 CacheConn String String String (Promise Int)

hget :: forall e. CacheConn -> String -> String -> CacheAff e (Either Error (Maybe String))
hget cacheConn key field = attempt <<< map readStringMaybe <<< toAff $ runFn3 hgetJ cacheConn key field

hset :: forall e. CacheConn -> String -> String -> String -> CacheAff e (Either Error Boolean)
hset cacheConn key field value = attempt <<< map isNotZero <<< toAff $ runFn4 hsetJ cacheConn key field value
