module Cache.Types where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Prelude (class Show)

foreign import data CacheConn :: Type

foreign import data CACHE :: Effect

data CacheConnOpts

type CacheEff e = Eff (cache :: CACHE | e)
type CacheAff e = Aff (cache :: CACHE | e)

data SetOptions = NoOptions
                | IfNotExist
                | IfExist

instance showSetOptions :: Show SetOptions where
  show NoOptions  = ""
  show IfNotExist = "NX"
  show IfExist    = "XX"
