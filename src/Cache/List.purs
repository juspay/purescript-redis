module Cache.List
  ( lindex
  , lpop
  , lpush
  , rpop
  , rpush
  ) where

import Cache.Internal (readStringMaybe)
import Cache.Types (class CacheConn, CacheAff)
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (Error)
import Control.Promise (Promise, toAff)
import Data.Either (Either)
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe)
import Prelude (map, ($), (<<<))

foreign import rpopJ :: forall a. Fn2 a String (Promise Foreign)
foreign import rpushJ :: forall a. Fn3 a String String (Promise Int)
foreign import lpopJ :: forall a. Fn2 a String (Promise Foreign)
foreign import lpushJ :: forall a. Fn3 a String String (Promise Int)
foreign import lindexJ :: forall a. Fn3 a String Int (Promise Foreign)

rpop :: forall a e. CacheConn a => a -> String -> CacheAff e (Either Error (Maybe String))
rpop cacheConn listName = attempt <<< map readStringMaybe <<< toAff $ runFn2 rpopJ cacheConn listName

rpush :: forall a e. CacheConn a => a -> String -> String -> CacheAff e (Either Error Int)
rpush cacheConn listName value = attempt <<< toAff $ runFn3 rpushJ cacheConn listName value

lpop :: forall a e. CacheConn a => a -> String -> CacheAff e (Either Error (Maybe String))
lpop cacheConn listName = attempt <<< map readStringMaybe <<< toAff $ runFn2 lpopJ cacheConn listName

lpush :: forall a e. CacheConn a => a -> String -> String -> CacheAff e (Either Error Int)
lpush cacheConn listName value = attempt <<< toAff $ runFn3 lpushJ cacheConn listName value

lindex :: forall a e. CacheConn a => a -> String -> Int -> CacheAff e (Either Error (Maybe String))
lindex cacheConn listName index = attempt <<< map readStringMaybe <<< toAff $ runFn3 lindexJ cacheConn listName index
