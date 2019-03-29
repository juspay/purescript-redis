module Test.RedisStreams where

import Prelude
import Cache (CacheConn)
import Control.Monad.Aff (Aff)

redisStreamTest :: forall e. CacheConn -> Aff e Unit
redisStreamTest cacheConn = pure unit