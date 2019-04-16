module Cache.Multi
  ( Multi
  , delMulti
  , execMulti
  , expireMulti
  , getMulti
  , hgetMulti
  , hsetMulti
  , incrMulti
  , lindexMulti
  , lpopMulti
  , lpushMulti
  , newMulti
  , publishMulti
  , rpopMulti
  , rpushMulti
  , setMulti
  , subscribeMulti
  ) where

import Cache.Types (CacheConn, SetOptions)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Promise (Promise, toAff)
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Either (Either)
import Data.Int (round)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..), Seconds)
import Prelude (show, ($), (<<<))

foreign import data Multi :: Type

foreign import newMultiJ :: forall e. CacheConn -> Eff e Multi

foreign import setMultiJ :: forall e. String -> String -> String -> String -> Multi -> Eff e Multi
foreign import getMultiJ :: forall e.  String -> Multi -> Eff e Multi
foreign import delMultiJ :: forall e. Array String -> Multi -> Eff e Multi
foreign import expireMultiJ :: forall e. String -> Int -> Multi -> Eff e Multi
foreign import incrMultiJ :: forall e.  String -> Multi -> Eff e Multi
foreign import hsetMultiJ :: forall e. String -> String -> String -> Multi -> Eff e Multi
foreign import hgetMultiJ :: forall e. String -> String -> Multi -> Eff e Multi
foreign import publishMultiJ :: forall e. String -> String -> Multi -> Eff e Multi
foreign import subscribeMultiJ :: forall e. String -> Multi -> Eff e Multi
foreign import rpopMultiJ :: forall e. String -> Multi -> Eff e Multi
foreign import rpushMultiJ :: forall e. String -> String -> Multi -> Eff e Multi
foreign import lpopMultiJ :: forall e. String -> Multi -> Eff e Multi
foreign import lpushMultiJ :: forall e. String -> String -> Multi -> Eff e Multi
foreign import lindexMultiJ :: forall e. String -> Int -> Multi -> Eff e Multi
foreign import execMultiJ :: Multi -> Promise (Array String)

newMulti :: forall e. CacheConn -> Eff e Multi
newMulti = newMultiJ

execMulti :: forall e. Multi -> Aff e (Either Error (Array String))
execMulti = attempt <<< toAff <<< execMultiJ

setMulti :: forall e. String -> String -> Maybe Milliseconds -> SetOptions -> Multi -> Eff e Multi
setMulti key value mExp opts = setMultiJ key value (maybe "" msToString mExp) (show opts)
  where
        msToString (Milliseconds v) = show $ round v

getMulti :: forall e. String -> Multi -> Eff e Multi
getMulti val = getMultiJ val

delMulti :: forall e. NonEmptyArray String -> Multi -> Eff e Multi
delMulti keys = delMultiJ (toArray keys)

expireMulti :: forall e. String -> Seconds -> Multi -> Eff e Multi
expireMulti key ttl = expireMultiJ key (round <<< unwrap $ ttl)

incrMulti :: forall e. String -> Multi -> Eff e Multi
incrMulti key = incrMultiJ key

hsetMulti :: forall e. String -> String -> String -> Multi -> Eff e Multi
hsetMulti key field val = hsetMultiJ key field val

hgetMulti :: forall e. String -> String -> Multi -> Eff e Multi
hgetMulti key field = hgetMultiJ key field

publishMulti :: forall e. String -> String -> Multi -> Eff e Multi
publishMulti channel message = publishMultiJ channel message

subscribeMulti :: forall e. String -> Multi -> Eff e Multi
subscribeMulti channel = subscribeMultiJ channel

rpopMulti :: forall e. String -> Multi -> Eff e Multi
rpopMulti listName = rpopMultiJ listName

rpushMulti :: forall e. String -> String -> Multi -> Eff e Multi
rpushMulti listName value = rpushMultiJ listName value

lpopMulti :: forall e. String -> Multi -> Eff e Multi
lpopMulti listName = lpopMultiJ listName

lpushMulti :: forall e. String -> String -> Multi -> Eff e Multi
lpushMulti listName value = lpushMultiJ listName value

lindexMulti :: forall e. String -> Int -> Multi -> Eff e Multi
lindexMulti listName index = lindexMultiJ listName index
