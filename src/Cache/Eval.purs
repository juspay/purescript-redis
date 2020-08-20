module Cache.Eval 
 (
   defineCommand
  , runCommand
  , eval
 ) where

import Prelude

import Cache (class CacheConn)
import Control.Promise (Promise, toAff)
import Data.Either (Either)
import Data.Function.Uncurried (Fn4, runFn4)
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt)
import Effect.Class (liftEffect)
import Foreign (Foreign)

foreign import defineCommandJ :: forall a. Fn4 a String Int String (Effect Unit)
foreign import runCommandJ :: forall a. Fn4 a String (Array String) (Array String) (Promise Foreign)
foreign import evalJ :: forall a. Fn4 a String (Array String) (Array String) (Promise Foreign)

-- defineCommand is ioredis api which caches the script using EVALSHA. Usefull when the script has to be run multiple times.
defineCommand :: forall a. CacheConn a => a -> String -> Int -> String -> Aff (Either Error Unit)
defineCommand cacheConn scriptName numberOfKeys script = attempt <<< liftEffect $ runFn4 defineCommandJ cacheConn scriptName numberOfKeys script

-- runCommand is ioredis api which executes the script cached by `defineCommand`.
runCommand :: forall a. CacheConn a => a -> String -> Array String -> Array String -> Aff (Either Error Foreign)
runCommand cacheConn scriptName keys args = attempt <<< toAff $ runFn4 runCommandJ cacheConn scriptName keys args

-- eval is redis EVAL api which runs the script without caching it.
eval :: forall a. CacheConn a => a -> String -> Array String -> Array String -> Aff (Either Error Foreign)
eval cacheConn script keys args = attempt <<< toAff $ runFn4 evalJ cacheConn script keys args