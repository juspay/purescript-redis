module Cache.Eval 
 (
   defineCommand
  , runCommand
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

foreign import defineCommandJ :: forall a. Fn4 a String Int String (Effect Foreign)
foreign import runCommandJ :: forall a. Fn4 a String (Array String) (Array String) (Promise Foreign)

defineCommand :: forall a. CacheConn a => a -> String -> Int -> String -> Aff (Either Error Foreign)
defineCommand cacheConn scriptName numberOfKeys script = attempt <<< liftEffect $ runFn4 defineCommandJ cacheConn scriptName numberOfKeys script

runCommand :: forall a. CacheConn a => a -> String -> Array String -> Array String -> Aff (Either Error Foreign)
runCommand cacheConn scriptName keys args = attempt <<< toAff $ runFn4 runCommandJ cacheConn scriptName keys args