module Cache.Types where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Data.BigInt (BigInt, toString)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Tuple (Tuple)
import Prelude (class Eq, class Show, (<>))

-- Represent a JavaScript object on which we can use standard Redis commands as
-- functions. The intention is to represent both single-node and cluster
-- connections as equivalent types that can be used with most functions. This
-- doesn't work with Multi as that does not return the same value as these two
-- types.
class CacheConn a

-- General types

foreign import data SimpleConn :: Type

instance simpleConnCacheConn :: CacheConn SimpleConn

foreign import data CACHE :: Effect

data SimpleConnOpts

type CacheEff e = Eff (cache :: CACHE | e)
type CacheAff e = Aff (cache :: CACHE | e)


-- Stream-related types

data SetOptions = NoOptions
                | IfNotExist
                | IfExist

instance showSetOptions :: Show SetOptions where
  show NoOptions  = ""
  show IfNotExist = "NX"
  show IfExist    = "XX"

type Item = Tuple String String

data EntryID = EntryID BigInt BigInt
             | AutoID
             | AfterLastID
             | MinID
             | MaxID
             | NewID

derive instance genericEntryID :: Generic EntryID _
instance eqEntryID :: Eq EntryID where
    eq = genericEq

data Entry = Entry EntryID (Array Item)

instance showEntryID :: Show EntryID where
  show :: EntryID -> String
  show (EntryID ms seq) = toString ms <> "-" <> toString seq
  show (AutoID        ) = "*"
  show (AfterLastID   ) = "$"
  show (MinID         ) = "-"
  show (MaxID         ) = "+"
  show (NewID         ) = ">"

data TrimStrategy = Maxlen

instance showTrimStrategy :: Show TrimStrategy where
  show Maxlen = "MAXLEN"


-- Cluster-related types

data ClusterConnOpts

type ClusterHost = { port :: Int, host :: String }

foreign import data ClusterConn :: Type

instance clusterConnCacheConn :: CacheConn ClusterConn
