module Cache.Types where

import Data.BigInt (BigInt, toString)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Tuple (Tuple)
import Prelude (class Eq, class Show, (<>))

foreign import data CacheConn :: Type

-- foreign import data CACHE :: Effect

data CacheConnOpts

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
