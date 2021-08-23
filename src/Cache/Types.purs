module Cache.Types where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Data.BigInt (BigInt, fromString, toString)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Tuple (Tuple)
import Prelude (class Eq, class Show, (<>))
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import Data.Foldable (foldMap)
import Data.Array (singleton)
import Data.Bifoldable (bifoldMap)

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

instance showEntryID :: Show EntryID where
  show :: EntryID -> String
  show (EntryID ms seq) = toString ms <> "-" <> toString seq
  show (AutoID        ) = "*"
  show (AfterLastID   ) = "$"
  show (MinID         ) = "-"
  show (MaxID         ) = "+"
  show (NewID         ) = ">"

instance encodeEntryID :: Encode EntryID where 
  encode (EntryID ms seq) = encode ((toString ms) <> " " <> (toString seq))
  encode id = encode id

instance decodeEntryID :: Decode EntryID where
  decode id = decode id

itemsToArray :: Array Item -> Array String
itemsToArray = foldMap (bifoldMap singleton singleton)

data Entry = Entry EntryID (Array Item)

derive instance genericEntry :: Generic Entry _

instance encodeEntry :: Encode Entry where
  encode (Entry entryID arr) = encode (itemsToArray arr)

instance decodeEntry :: Decode Entry where
  decode entry = decode entry

data TrimStrategy = Maxlen

instance showTrimStrategy :: Show TrimStrategy where
  show Maxlen = "MAXLEN"


-- Cluster-related types

data ClusterConnOpts

type ClusterHost = { port :: Int, host :: String }

foreign import data ClusterConn :: Type

instance clusterConnCacheConn :: CacheConn ClusterConn