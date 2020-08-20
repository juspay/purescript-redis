module Cache.Cluster
  ( clusterRetryStrategy
  , enableReadyCheck
  , newClusterConn
  , password
  , scaleReads
  ) where

import Cache.Types (ClusterConn, ClusterConnOpts, ClusterHost)
import Control.Promise (Promise, toAff)
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Options (Option, Options, opt, options)
import Effect.Aff (Aff, Error, attempt)
import Foreign (Foreign)
import Prelude (($), (<<<))

clusterRetryStrategy :: Option ClusterConnOpts String
clusterRetryStrategy = opt "clusterRetryStrategy"

password :: Option ClusterConnOpts String
password = opt "password"

enableReadyCheck :: Option ClusterConnOpts Boolean
enableReadyCheck = opt "enableReadyCheck"

scaleReads :: Option ClusterConnOpts String
scaleReads = opt "scaleReads"

foreign import newClusterConnJ :: Fn2 (Array ClusterHost) Foreign (Promise ClusterConn)

newClusterConn :: Array ClusterHost -> Options ClusterConnOpts -> Aff (Either Error ClusterConn)
newClusterConn hosts opts = attempt <<< toAff $ runFn2 newClusterConnJ hosts (options opts)
