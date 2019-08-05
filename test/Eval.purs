module Test.Eval where

import Prelude

import Cache (class CacheConn, del)
import Cache.Eval (defineCommand, runCommand)
import Cache.SortedSet (zpopmin)
import Control.Monad.Except (runExcept)
import Data.Array.NonEmpty (singleton)
import Data.Either (Either(..), either)
import Data.Tuple (Tuple(..))
import Foreign (unsafeToForeign)
import Foreign.Generic (decode)
import Test.Internal (checkRight, checkValue)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

testKey :: String
testKey = "test-eval"

testId :: String
testId = "test-uniqueId"

testId1 :: String
testId1 = "test-uniqueId-1"

luaScript1 :: String
luaScript1 = """
  return redis.call('zadd', KEYS[1], ARGV[1], ARGV[2], ARGV[3], ARGV[4])
"""

evalTest :: forall a. CacheConn a => a -> Spec Unit
evalTest cacheConn =
  describe "Eval" do
    it "works" do
      _  <- del cacheConn $ singleton testKey

      v0 <- defineCommand cacheConn "zaddScript" 1 luaScript1
      checkRight v0

      v1 <- runCommand cacheConn "zaddScript" [ testKey ] [ "4", testId, "1", testId1 ]
      case v1 of
        Right val -> 
          decode val
          # runExcept
          # either (const $ fail "Not expected") 
            (\(x::Int) -> if x == 2 then pure unit
                else fail "Value not set"
            )
        Left err -> fail $ show err

      v3 <- zpopmin cacheConn testKey 1
      checkValue v3 [ Tuple testId1 "1" ]

      _  <- del cacheConn $ singleton testKey
      pure unit