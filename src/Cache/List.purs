module Cache.List
  ( lindex
  , lpop
  , lpush
  , rpop
  , rpush
  ) where

import Cache.Internal (readStringMaybe)
import Cache.Types (CacheConn)
import Control.Promise (Promise, toAff)
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe)
import Effect.Aff (Aff, attempt)
import Effect.Exception (Error)
import Foreign (Foreign)
import Prelude (map, ($), (<<<))

foreign import rpopJ :: Fn2 CacheConn String (Promise Foreign)
foreign import rpushJ :: Fn3 CacheConn String String (Promise Int)
foreign import lpopJ :: Fn2 CacheConn String (Promise Foreign)
foreign import lpushJ :: Fn3 CacheConn String String (Promise Int)
foreign import lindexJ :: Fn3 CacheConn String Int (Promise Foreign)

rpop :: CacheConn -> String -> Aff (Either Error (Maybe String))
rpop cacheConn listName = attempt <<< map readStringMaybe <<< toAff $ runFn2 rpopJ cacheConn listName

rpush :: CacheConn -> String -> String -> Aff (Either Error Int)
rpush cacheConn listName value = attempt <<< toAff $ runFn3 rpushJ cacheConn listName value

lpop :: CacheConn -> String -> Aff (Either Error (Maybe String))
lpop cacheConn listName = attempt <<< map readStringMaybe <<< toAff $ runFn2 lpopJ cacheConn listName

lpush :: CacheConn -> String -> String -> Aff (Either Error Int)
lpush cacheConn listName value = attempt <<< toAff $ runFn3 lpushJ cacheConn listName value

lindex :: CacheConn -> String -> Int -> Aff (Either Error (Maybe String))
lindex cacheConn listName index = attempt <<< map readStringMaybe <<< toAff $ runFn3 lindexJ cacheConn listName index